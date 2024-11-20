# Start by looking at the `big_num` class

node_or_na <- function(value) is.na(value) || S7_inherits(value, node) # predicate to check if `value` is a `node` or `NA`, pulled out to reduce duplication

node <- new_class("node",
  package = "BigNum",
  properties = list(
    VALUE = new_property(
      class_numeric, # TODO: make this a generic?
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

    new_object(S7_object(), VALUE = VALUE, e = rlang::new_environment(list(nxt = nxt)))
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
        warning("@head should not be set manually. Maybe you meant to use `append_to_start()`?", call. = FALSE)

        my_env <- self@e
        my_env$head <- value

        self
      },
      validator = node_or_na
    ),
    tail = new_property(node,
      getter = function(self) self@e$tail,
      setter = function(self, value) {
        warning("@tail should not be set manually", call. = FALSE)

        my_env <- self@e
        my_env$tail <- value

        self
      }
    ),
    length = new_property(class_integer, getter = function(self) self@e$length, setter = function(self, value) {
      warning("@length should not be set manually", call. = FALSE)
      environ <- self@e
      environ$length <- as.integer(value)

      self
    }),
    e = new_property(class_environment, getter = function(self) self@e)
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
      return(new_object(S7_object(), e = rlang::new_environment(list(head = NA, tail = NA, length = 0))))
    }

    extract_digit_to_node <- function(len) node(as.integer(substring(num, len, len))) # TODO: throw error if the char isn't numeric

    len <- nchar(num)
    nodes <- lapply(len:1, function(i) extract_digit_to_node(i))

    if (len != 1) {
      for (i in (len - 1):1) {
        nodes[[i]]@nxt <- nodes[[i + 1]]
      }
    }

    e <- rlang::new_environment(list(
      head = nodes[[1]],
      tail = nodes[[len]],
      length = len
    ))

    new_object(S7_object(), e = e)
  }
)

#' Infinite precision natural number using a singly linked list
#'
#' @description
#' BigNum exposes three S7 objects, which are all closely related,
#' i.e. they are all tightly coupled--which isn't great OOP design
#' but will suffice.
#'
#' The `big_num` class is essentially a wrapper around [linked_list]
#' with a custom print method as well as some defined operators such as
#' `+`, `*`, and `^` with an integer.
#'
#' @export
#' @param num A string representation of a num.
big_num <- new_class("big_num", # this defines the class, and should match the name of the object on the left-hand-side of the assignment.
  package = "BigNum", # this defines a package for this `big_num` object so that it doesn't conflict with other potential `big_num` objects from other packages.
  # properties defines a list of variables that `big_num`s have. Here we have just one property, a `linked_list`
  properties = list(
    ll = new_property(linked_list, getter = function(self) self@ll) # this property has a custom getter, but no setter. This means that you can get a `big_num`'s `ll` but you cannot set it--i.e. this is a read-only property.
  ),
  # the class validator is a function that takes a purported `big_num` object (viz. `self`), and runs some checks to ensure that it actually is a `big_num`.
  validator = function(self) {
    # as expected, this predicate just checks if `self`'s `ll` is an actual `linked_list`
    if (!S7_inherits(self@ll, linked_list)) {
      "@ll must be a valid linked list."
    }
    # all validators should return a string if there is an error, or NULL if all's good. Let R return NULL for you automatically.
  },
  # S7 objects can have custom constructors which allow you to do some logic with inputs before ending with a call to `new_object()`
  # this constructor finesses the input to allow for construction of `big_num`s with a numeric `num` as well as the expected character input
  # note as well that the constructor is a function, and so it takes certain arguments which can have default values. Here, all `big_nums` start
  # out empty. However, looking at the `node` class one can note that `val` has no default and so must be provided when creating a `node` object.
  constructor = function(num = "") {
    force(num)
    new_object(S7_object(), ll = linked_list(format(num, trim = TRUE, scientific = FALSE)))
  }
)

is.na <- new_external_generic("base", "is.na", "x")
method(is.na, node) <- function(x) !S7_inherits(x, node)

is_even <- new_generic("is_even", c("x"))
method(is_even, big_num) <- function(x) x@ll@head@VALUE %% 2 == 0

append <- new_generic("append", c("x", "ll"))
method(append, list(node, linked_list)) <- function(x, ll) {
  suppressWarnings({
    if (is.na(ll@head)) {
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
method(append, list(class_numeric, linked_list)) <- function(x, ll) {
  stopifnot(x < 10 && x >= 0)
  append(node(x), ll)
}
method(append, list(class_numeric | node, big_num)) <- function(x, ll) {
  append(x, ll@ll)
}

append_to_start <- new_generic("append_to_start", c("x", "ll"))
method(append_to_start, list(node, linked_list)) <- function(x, ll) {
  suppressWarnings({
    if (is.na(ll@head)) {
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

  stack <- character(len)
  current <- x@ll@head

  for (i in len:1) {
    stack[i] <- current@VALUE
    current <- current@nxt
  }

  string <- paste0(stack, collapse = "")
  cat(string, "\n")

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


# function to remove tail zeros from a `big_num`
remove_leading_zeros <- function(bn) {
  ll <- bn@ll
  # TODO: length is 1 less than it should be still
  if (ll@length <= 1) {
    return(bn)
  }

  current <- ll@head
  last_nonzero <- NA
  final_length <- 0

  while (!is.na(current)) {
    if (current@VALUE != 0) {
      last_nonzero <- current
      final_length <- ll@length
    } else {
      final_length <- final_length - 1
    }
    current <- current@nxt
  }

  if (is.na(last_nonzero)) {
    suppressWarnings({
      ll@head@nxt <- NA
      ll@tail <- ll@head
      ll@length <- 1
    })

    return(invisible(bn))
  }

  last_nonzero@nxt <- NA
  suppressWarnings({
    ll@tail <- last_nonzero
    ll@length <- final_length
  })

  invisible(bn)
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

  remove_leading_zeros(product)
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
method(`==`, list(big_num, big_num)) <- function(e1, e2) {
  e1@ll == e2@ll
}
method(`==`, list(big_num, class_numeric)) <- function(e1, e2) {
  e1 == big_num(e2)
}
method(`==`, list(class_numeric, big_num)) <- function(e1, e2) {
  big_num(e1) == e2
}
