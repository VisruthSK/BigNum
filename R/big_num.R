# TODO: pull out latter part of statement and rewrite as `is_node(x)`
node_or_na <- function(value) S7_inherits(value, node) || is.na(value) # predicate to check if `value` is a `node` or `NA`, pulled out to reduce duplication
# TODO: change to use unions if possible? `NA | node`?

node <- new_class("node",
  package = "BigNum",
  properties = list(
    VALUE = new_property(
      class_numeric, # TODO: make this a generic?
      getter = function(self) self@VALUE,
      setter = NULL
    ),
    state = new_property(
      class_environment,
      getter = function(self) self@state,
      setter = NULL
    ),
    nxt = new_property(class_any,
      getter = function(self) {
        self@state$nxt
      },
      setter = function(self, value) {
        my_env <- self@state
        my_env$nxt <- value

        self
      }
    )
  ),
  validator = function(self) {
    if (!node_or_na(self@nxt)) {
      "@nxt must be a `node` object or `NA`."
    } else if (!is.numeric(self@VALUE) || length(self@VALUE) != 1) {
      "@VALUE must be a `numeric` vector of length 1."
    }
  },
  constructor = function(VALUE, nxt = NA) {
    force(VALUE)
    force(nxt)

    new_object(S7_object(), VALUE = VALUE, state = rlang::new_environment(list(nxt = nxt)))
  }
)

linked_list <- new_class("linked_list",
  package = "BigNum",
  properties = list(
    head = new_property(
      node,
      getter = function(self) self@state$head,
      setter = function(self, value) {
        # TODO: kick up to error?
        warning("@head should not be set manually. Did you mean to use `append_to_start()`?", call. = FALSE)

        my_env <- self@state
        my_env$head <- value

        self
      },
      validator = node_or_na
    ),
    tail = new_property(node,
      getter = function(self) self@state$tail,
      setter = function(self, value) {
        warning("@tail should not be set manually", call. = FALSE)

        my_env <- self@state
        my_env$tail <- value

        self
      }
    ),
    length = new_property(class_integer, getter = function(self) self@state$length, setter = function(self, value) {
      warning("@length should not be set manually", call. = FALSE)
      environ <- self@state
      environ$length <- as.integer(value)

      self
    }),
    state = new_property(class_environment, getter = function(self) self@state)
  ),
  validator = function(self) {
    if (!node_or_na(self@head)) {
      "@head must be a `node` object or `NA`."
    } else if (!node_or_na(self@tail)) {
      "@tail must be a `node` object or `NA`."
    }
  },
  constructor = function(num = NULL) {
    force(num)

    if (is.null(num) || num == "") {
      return(new_object(S7_object(), state = rlang::new_environment(list(head = NA, tail = NA, length = 0))))
    }

    extract_digit_to_node <- function(pos) node(as.integer(substring(num, pos, pos))) # TODO: throw error if the char isn't numeric

    len <- nchar(num)
    nodes <- lapply(len:1, function(i) extract_digit_to_node(i))

    if (len != 1) {
      for (i in (len - 1):1) {
        nodes[[i]]@nxt <- nodes[[i + 1]]
      }
    }

    state <- rlang::new_environment(list(
      head = nodes[[1]],
      tail = nodes[[len]],
      length = len
    ))

    new_object(S7_object(), state = state)
  }
)

big_num <- new_class("big_num",
  parent = linked_list,
  package = "BigNum",
  constructor = function(num = "") {
    if (!is.character(num)) warning("Use strings to make big_nums.", call. = FALSE)
    new_object(linked_list(num))
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
