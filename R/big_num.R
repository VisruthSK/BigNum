node_or_na <- function(value) is.na(value) || S7_inherits(value, node) # predicate to check if `nd` is a node or NA

node <- new_class("node",
  package = "BigNum",
  properties = list(
    VALUE = new_property(
      class_numeric, # TODO: make this a generic? Swap to integers here?
      getter = function(self) self@VALUE
    ),
    e = new_property(class_environment, getter = function(self) self@e),
    nxt = new_property(class_any,
      getter = function(self) {
        self@e$nxt
      },
      setter = function(self, value) {
        my_env <- self@e
        my_env$nxt <- value

        self
      }
    )
  ),
  # TODO: fix this validator too
  validator = function(self) {
    if (!node_or_na(self@nxt)) {
      "@nxt must be a `node` object or `NA`."
    }
    if (!is.numeric(self@VALUE) || length(self@VALUE) != 1) {
      "@VALUE must be a `numeric` vector of length 1."
    }
  },
  constructor = function(VALUE, nxt = NA) {
    force(VALUE)
    # if (VALUE != floor(VALUE)) warning("Coercing value to integer")
    VALUE <- as.integer(VALUE)
    force(nxt)
    new_object(S7_object(), VALUE = VALUE, e = rlang::env(nxt = nxt))
  }
)

linked_list <- new_class("linked_list",
  package = "BigNum",
  properties = list(
    head = new_property(
      node,
      getter = function(self) self@e$head,
      setter = function(self, value) {
        # TODO: kick up to error?
        warning("@head should not be set manually", call. = FALSE)

        self@e$head <- value
        self
      }
    ),
    tail = new_property(node,
      getter = function(self) self@e$tail,
      setter = function(self, value) {
        warning("@tail should not be set manually", call. = FALSE)

        self@e$tail <- value
        self
      }
    ),
    length = new_property(class_numeric, getter = function(self) self@e$length, setter = function(self, value) self@e$length <- value),
    e = new_property(class_environment)
  ), validator = function(self) {
    # TODO: validation might be broken?
    if (!node_or_na(self@head)) {
      "@head must be a `node` object or `NA`."
    } else if (!node_or_na(self@tail)) {
      "@tail must be a `node` object or `NA`."
    }
  }, constructor = function(num = NULL) {
    if (is.null(num) || num == "") {
      return(new_object(S7_object(), e = rlang::env(head = NA, tail = NA, length = 0)))
    }

    extract_digit_to_node <- function(len) node(as.integer(substring(num, len, len))) # TODO: throw error if the char isn't numeric

    len <- nchar(num)
    nodes <- lapply(len:1, function(i) extract_digit_to_node(i))

    if (len != 1) {
      for (i in (len - 1):1) {
        nodes[[i]]@nxt <- nodes[[i + 1]]
      }
    }

    e <- rlang::env(head = nodes[[1]], tail = nodes[[len]], length = len)

    S7::new_object(S7::S7_object(), e = e)
  }
)

#' Makes a BigNum
#'
#' @param num A character vector representing a number.
#'
#' @return A big_num S7 object.
#' @export
#'
#' @examples
big_num <- new_class("big_num",
  package = "BigNum",
  properties = list(
    ll = new_property(linked_list, getter = function(self) self@ll)
  ),
  constructor = function(num = "") {
    S7::new_object(S7::S7_object(), ll = linked_list(format(num, trim = TRUE, scientific = FALSE)))
  }
)

is.na <- new_external_generic("base", "is.na", "x")
method(is.na, node) <- function(x) !S7_inherits(x, node)

is_even <- new_generic("is_even", c("x"))
method(is_even, big_num) <- function(x) x@ll@head@VALUE %% 2 == 0

append <- new_generic("append", c("x", "ll"))
method(append, list(node, linked_list)) <- function(x, ll) {
  suppressWarnings(
    if (is.na(ll@head)) {
      ll@head <- x
      ll@tail <- x
    } else {
      ll@tail@nxt <- x
      ll@tail <- x
    }
  )

  # TODO: what does this error mean and why does this still work if ignored
  # the attributes do exist in the environment so I'm not using rlang::env properly
  # Error in `@<-.S7_object`:
  #   \! Tried to remove non-existent element from pairlist
  try(ll@length <- ll@length + 1, silent = TRUE)

  invisible(ll)
}
method(append, list(class_numeric, linked_list)) <- function(x, ll) {
  stopifnot(x < 10 && x >= 0)
  append(node(x), ll)
}
method(append, list(class_numeric | node, big_num)) <- function(x, ll) {
  append(x, ll@ll)
}

append_to_start <- new_generic("append_to_start", c("x", "ll"))
method(append_to_start, list(node, linked_list)) <- function(x, ll) {
  suppressWarnings(
    if (is.na(ll@head)) {
      ll@head <- x
      ll@tail <- x
    } else {
      x@nxt <- ll@head
      ll@head <- x
    }
  )

  try(ll@length <- ll@length + 1, silent = TRUE)

  invisible(ll)
}
method(append_to_start, list(class_numeric, linked_list)) <- function(x, ll) {
  stopifnot(x < 10 && x >= 0)
  append_to_start(node(x), ll)
}
method(append_to_start, list(class_numeric | node, big_num)) <- function(x, ll) {
  append_to_start(x, ll@ll)
}

print <- new_external_generic("base", "print", "x")
method(print, linked_list) <- function(x) {
  current <- x@head
  while (!is.na(current)) {
    cat(current@VALUE, "-> ")
    current <- current@nxt
  }
  cat("NA\n")

  invisible(x)
}
method(print, big_num) <- function(x) {
  len <- x@ll@length

  if (len == 0) {
    cat("NA\n")
    return(invisible(x))
  }

  # TODO: remove leading 0s?
  stack <- character(len)
  current <- x@ll@head

  for (i in len:1) {
    stack[i] <- current@VALUE
    current <- current@nxt
  }

  string <- paste0(stack, collapse = "")
  cat(string)

  invisible(x)
}

`+` <- new_external_generic("base", "+", c("e1", "e2"))
add_helper <- function(node1, node2, carry, sum) {
  digit <- node1@VALUE + node2@VALUE + carry
  carry <- digit %/% 10
  digit <- digit %% 10
  append(digit, sum)

  carry
}
method(`+`, list(big_num, big_num)) <- function(e1, e2) {
  sum <- big_num()
  node1 <- e1@ll@head
  node2 <- e2@ll@head
  carry <- 0

  while (!is.na(node1) && !is.na(node2)) {
    carry <- add_helper(node1, node2, carry, sum)
    node1 <- node1@nxt
    node2 <- node2@nxt
  }
  while (!is.na(node1)) {
    carry <- add_helper(node1, node(0), carry, sum)
    node1 <- node1@nxt
  }
  while (!is.na(node2)) {
    carry <- add_helper(node2, node(0), carry, sum)
    node2 <- node2@nxt
  }
  if (carry > 0) {
    append(carry, sum)
  }

  sum
}
method(`+`, list(big_num, class_numeric)) <- function(e1, e2) {
  e1 + big_num(e2)
}
method(`+`, list(class_numeric, big_num)) <- function(e1, e2) {
  big_num(e1) + e2
}

`*` <- new_external_generic("base", "*", c("e1", "e2"))
method(`*`, list(big_num, big_num)) <- function(e1, e2) {
  product <- big_num(0)
  node2 <- e2@ll@head
  shift2 <- 0
  while (!is.na(node2)) {
    node1 <- e1@ll@head
    shift1 <- 0
    while (!is.na(node1)) {
      temp <- big_num(node1@VALUE * node2@VALUE)
      replicate(shift1 + shift2, append_to_start(0, temp))
      product <- product + temp
      node1 <- node1@nxt
      shift1 <- shift1 + 1
    }
    node2 <- node2@nxt
    shift2 <- shift2 + 1
  }

  product
}
method(`*`, list(big_num, class_numeric)) <- function(e1, e2) {
  e1 * big_num(e2)
}
method(`*`, list(class_numeric, big_num)) <- function(e1, e2) {
  big_num(e1) * e2
}

`^` <- new_external_generic("base", "^", c("e1", "e2"))
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

`==` <- new_external_generic("base", "==", c("e1", "e2"))
method(`==`, list(big_num, big_num)) <- function(e1, e2) {
  len <- e1@ll@length
  if (len != e2@ll@length) {
    return(FALSE)
  }
  node1 <- e1@ll@head
  node2 <- e2@ll@head
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

