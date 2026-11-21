# define a helper function for properties that cannot be modified
readonly <- function(class) {
  new_property(
    class = class,
    getter = function(self, name) prop(self, name)
  )
}

# define a stub for the `node` class since a `node`
# contains a reference (`@nxt`) to another `node`
node_stub <- new_external_class("BigNum", "node")
node := new_class(
  parent = class_environment,
  properties = list(
    VALUE = readonly(class_numeric),
    nxt = NULL | node_stub
  ),
  constructor = function(VALUE, nxt = NULL) {
    new_object(
      new.env(parent = emptyenv()),
      VALUE = VALUE,
      nxt = nxt
    )
  }
)

linked_list := new_class(
  # S7 1.0.0 added experimental support for
  # environment semantics, which offer a very
  # nice way to have a mutable struct.
  parent = class_environment,
  properties = list(
    head = readonly(NULL | node),
    tail = readonly(NULL | node),
    length = readonly(class_integer)
  ),
  constructor = function(nodes = list()) {
    if (!all(vapply(nodes, S7_inherits, logical(1), class = node))) {
      stop("All elements must be <node> instances")
    }
    n <- length(nodes)

    if (n > 1) {
      for (i in seq_len(n - 1)) {
        nodes[[i]]@nxt <- nodes[[i + 1]]
      }
    }

    new_object(
      new.env(parent = emptyenv()),
      head = if (n) nodes[[1]] else NULL,
      tail = if (n) nodes[[n]] else NULL,
      length = n
    )
  }
)

big_num := new_class(
  # a `big_num` object is also a `linked_list` object
  # which means it has all the same properties and methods
  parent = linked_list,
  constructor = function(num_str = "") {
    # store a number backwards internally for maths:
    # 1234 is stored as "4" -> "3" -> "2" -> "1" -> NA
    new_object(
      linked_list(
        lapply(
          as.integer(
            rev(
              strsplit(as.character(num_str), "")[[1L]]
            )
          ),
          node
        )
      )
    )
  }
)

# define a generic, and a method for that generic on `big_num` objects
is_even := new_generic("x")
method(is_even, big_num) <- function(x) x@head@VALUE %% 2 == 0

.insert_node <- function(x, values, at_start) {
  if (!is.null(values@nxt) || identical(values, x@tail)) {
    stop("Node is already linked")
  }

  if (is.null(x@head)) {
    attr(x, "head") <- values
    attr(x, "tail") <- values
  } else if (at_start) {
    values@nxt <- x@head
    attr(x, "head") <- values
  } else {
    tail <- x@tail
    tail@nxt <- values
    attr(x, "tail") <- values
  }

  attr(x, "length") <- x@length + 1L

  invisible(x)
}

append := new_generic(c("x", "values"))
method(append, list(linked_list, node)) <- function(x, values, ...) {
  .insert_node(x, values, FALSE)
}

append_to_start := new_generic(c("x", "values"))
method(append_to_start, list(linked_list, node)) <- function(x, values, ...) {
  .insert_node(x, values, TRUE)
}

method(print, linked_list) <- function(x, ...) {
  current <- x@head
  while (S7_inherits(current)) {
    cat(current@VALUE, "-> ")
    current <- current@nxt
  }
  cat("NA\n")

  invisible(x)
}
method(print, big_num) <- function(x, ...) {
  len <- x@length

  if (len == 0) {
    cat("NA\n")
    return(invisible(x))
  }

  stack <- character(len)
  current <- x@head

  for (i in len:1) {
    stack[i] <- current@VALUE
    current <- current@nxt
  }

  string <- paste0(stack, collapse = "")
  cat(string, "\n")

  invisible(x)
}

add_helper <- function(node1, node2, carry, sum) {
  digit <- node1@VALUE + node2@VALUE + carry
  carry <- digit %/% 10
  digit <- digit %% 10
  append(sum, node(as.numeric(digit)))

  carry
}

# S7 supports multiple dispatch, so here we define addition
# for a, b when both are `big_num` objects, or only one is.
method(`+`, list(big_num, big_num)) <- function(e1, e2) {
  sum <- big_num()
  node1 <- e1@head
  node2 <- e2@head
  carry <- 0

  while (S7_inherits(node1) && S7_inherits(node2)) {
    carry <- add_helper(node1, node2, carry, sum)
    node1 <- node1@nxt
    node2 <- node2@nxt
  }
  while (S7_inherits(node1)) {
    carry <- add_helper(node1, node(0), carry, sum)
    node1 <- node1@nxt
  }
  while (S7_inherits(node2)) {
    carry <- add_helper(node2, node(0), carry, sum)
    node2 <- node2@nxt
  }
  if (carry > 0) {
    append(sum, node(as.integer(carry)))
  }

  sum
}
method(`+`, list(big_num, class_numeric)) <- function(e1, e2) {
  e1 + big_num(e2)
}
method(`+`, list(class_numeric, big_num)) <- function(e1, e2) {
  big_num(e1) + e2
}

.remove_leading_zeros <- function(bn) {
  if (is.null(bn@head)) {
    append(bn, node(0))
    return(bn)
  }

  current <- bn@head
  last_nonzero <- NULL
  last_index <- 0L
  index <- 0L

  while (!is.null(current)) {
    index <- index + 1L

    if (current@VALUE != 0) {
      last_nonzero <- current
      last_index <- index
    }

    current <- current@nxt
  }

  if (is.null(last_nonzero)) {
    first <- bn@head
    first@nxt <- NULL

    attr(bn, "tail") <- first
    attr(bn, "length") <- 1L
  } else {
    last_nonzero@nxt <- NULL

    attr(bn, "tail") <- last_nonzero
    attr(bn, "length") <- last_index
  }

  bn
}

method(`*`, list(big_num, big_num)) <- function(e1, e2) {
  product <- big_num("0")
  node2 <- e2@head
  shift2 <- 0

  while (!is.null(node2)) {
    node1 <- e1@head
    shift1 <- 0

    while (!is.null(node1)) {
      digit_product <- node1@VALUE * node2@VALUE
      temp <- big_num(as.character(digit_product))

      for (i in seq_len(shift1 + shift2)) {
        append_to_start(temp, node(0))
      }

      product <- product + temp

      node1 <- node1@nxt
      shift1 <- shift1 + 1
    }

    node2 <- node2@nxt
    shift2 <- shift2 + 1
  }

  .remove_leading_zeros(product)
}
method(`*`, list(big_num, class_numeric)) <- function(e1, e2) {
  e1 * big_num(e2)
}
method(`*`, list(class_numeric, big_num)) <- function(e1, e2) {
  big_num(e1) * e2
}

method(`^`, list(big_num, class_numeric)) <- function(e1, e2) {
  if (e2 < 0 || is.na(e2) || !is.finite(e2)) {
    stop("Exponent must be a non-negative integer")
  }

  result <- big_num("1")
  base <- e1
  n <- e2

  while (n > 0) {
    if (n %% 2 == 1) {
      result <- result * base
    }

    n <- n %/% 2

    if (n > 0) {
      base <- base * base
    }
  }

  result
}

method(`==`, list(linked_list, linked_list)) <- function(e1, e2) {
  len <- e1@length
  if (len != e2@length) {
    return(FALSE)
  }
  node1 <- e1@head
  node2 <- e2@head
  for (i in len:1) {
    if (node1@VALUE != node2@VALUE) {
      return(FALSE)
    }
    node1 <- node1@nxt
    node2 <- node2@nxt
  }
  TRUE
}
method(`==`, list(big_num, class_numeric)) <- function(e1, e2) {
  e1 == big_num(as.character(e2))
}
method(`==`, list(class_numeric, big_num)) <- function(e1, e2) {
  big_num(as.character(e1)) == e2
}
