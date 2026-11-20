same_type <- function(x, y) {
  identical(S7_class(x), S7_class(y))
}

node_stub <- new_external_class("BigNum", "node")
node := new_class(
  parent = class_environment,
  properties = list(
    VALUE = new_property(
      class_numeric,
      getter = function(self) self@VALUE,
      setter = NULL
    ),
    nxt = new_property(NULL | node_stub)
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
  parent = class_environment,
  properties = list(
    head = new_property(
      NULL | node,
      getter = function(self) self@head,
      setter = NULL
    ),
    tail = new_property(
      NULL | node,
      getter = function(self) self@tail,
      setter = NULL
    ),
    length = new_property(
      class_integer,
      getter = function(self) self@length,
      setter = NULL
    )
  ),
  constructor = function(nodes = list()) {
    if (!all(vapply(nodes, S7_inherits, logical(1), class = node))) {
      stop("All elements must be <node> instances")
    }
    n <- length(nodes)

    for (i in seq_len(n - 1)) {
      nodes[[i]]@nxt <- nodes[[i + 1]]
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
  parent = linked_list,
  constructor = function(num_str = "") {
    new_object(
      linked_list(
        lapply(
          as.integer(
            rev(
              strsplit(num_str, "")[[1L]]
            )
          ),
          node
        )
      )
    )
  }
)

is_even <- new_generic("is_even", c("x"))
method(is_even, big_num) <- function(x) x@head@VALUE %% 2 == 0

# TODO: rewrite to be external generic? `base::append()`
bn_append <- new_generic("bn_append", c("x", "ll"))
method(bn_append, list(node, linked_list)) <- function(x, ll) {
  suppressWarnings({
    if (!S7_inherits(ll@head)) {
      ll@head <- x
      ll@tail <- x
    } else {
      ll@tail@nxt <- x
      ll@tail <- x
    }
    ll@length <- ll@length + 1
  })

  invisible(ll)
}
method(bn_append, list(class_numeric, linked_list)) <- function(x, ll) {
  stopifnot(x < 10 && x >= 0)
  bn_append(node(x), ll)
}

append_to_start <- new_generic("append_to_start", c("x", "ll"))
method(append_to_start, list(node, linked_list)) <- function(x, ll) {
  suppressWarnings({
    if (!S7_inherits(ll@head)) {
      ll@head <- x
      ll@tail <- x
    } else {
      x@nxt <- ll@head
      ll@head <- x
    }
    ll@length <- ll@length + 1
  })

  invisible(ll)
}
method(append_to_start, list(class_numeric, linked_list)) <- function(x, ll) {
  stopifnot(x < 10 && x >= 0)
  append_to_start(node(x), ll)
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
  bn_append(digit, sum)

  carry
}
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
    bn_append(carry, sum)
  }

  sum
}
method(`+`, list(big_num, class_numeric)) <- function(e1, e2) {
  e1 + big_num(e2)
}
method(`+`, list(class_numeric, big_num)) <- function(e1, e2) {
  big_num(e1) + e2
}

# function to remove tail zeros from a `big_num`
remove_leading_zeros <- function(bn) {
  if (bn@length <= 1) {
    return(bn)
  }

  current <- bn@head
  last_nonzero <- NA
  final_length <- 0

  while (S7_inherits(current)) {
    if (current@VALUE != 0) {
      last_nonzero <- current
      final_length <- bn@length
    } else {
      final_length <- final_length - 1
    }
    current <- current@nxt
  }

  if (!S7_inherits(last_nonzero)) {
    suppressWarnings({
      bn@head@nxt <- NA
      bn@tail <- bn@head
      bn@length <- 1
    })

    return(invisible(bn))
  }

  last_nonzero@nxt <- NA
  suppressWarnings({
    bn@tail <- last_nonzero
    bn@length <- final_length
  })

  invisible(bn)
}

method(`*`, list(big_num, big_num)) <- function(e1, e2) {
  product <- big_num(0)
  node2 <- e2@head
  shift2 <- 0
  while (S7_inherits(node2)) {
    node1 <- e1@head
    shift1 <- 0
    while (S7_inherits(node1)) {
      temp <- big_num(node1@VALUE * node2@VALUE)
      replicate(shift1 + shift2, append_to_start(0, temp))
      product <- product + temp
      node1 <- node1@nxt
      shift1 <- shift1 + 1
    }
    node2 <- node2@nxt
    shift2 <- shift2 + 1
  }

  remove_leading_zeros(product)
}
method(`*`, list(big_num, class_numeric)) <- function(e1, e2) {
  e1 * big_num(e2)
}
method(`*`, list(class_numeric, big_num)) <- function(e1, e2) {
  big_num(e1) * e2
}

method(`^`, list(big_num, class_numeric)) <- function(e1, e2) {
  if (e2 == 0) {
    return(big_num(1))
  } else if (e2 == 1) {
    return(e1)
  } else if (e2 == 2) {
    return(e1 * e1)
  } else {
    if (e2 %% 2 == 1) {
      return((e1^2)^(e2 %/% 2))
    } else {
      return((e1^2)^(e2 %/% 2) * e1)
    }
  }
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
  e1 == big_num(e2)
}
method(`==`, list(class_numeric, big_num)) <- function(e1, e2) {
  big_num(e1) == e2
}
