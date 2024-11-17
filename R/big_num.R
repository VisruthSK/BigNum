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
    } else if (!node_or_na(self@head)) {
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

big_num <- new_class("big_num",
  package = "BigNum",
  properties = list(
    ll = new_property(linked_list, getter = function(self) self@ll)
  ),
  constructor = function(num = "") {
    S7::new_object(S7::S7_object(), ll = linked_list(num))
  }
)

is.na <- new_external_generic("base", "is.na", "x")
method(is.na, node) <- function(x) !S7_inherits(x, node)

append <- new_generic("append", "x", function(x, ll, ...) S7_dispatch())
method(append, node) <- function(x, ll) {
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
method(append, class_numeric) <- function(x, ll) {
  stopifnot(x < 10 && x >= 0)
  append(node(x), ll)
}

append_to_start <- new_generic("append_to_start", "x", function(x, ll, ...) S7_dispatch())
method(append_to_start, node) <- function(x, ll) {
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
method(append_to_start, class_numeric) <- function(x, ll) {
  stopifnot(x < 10 && x >= 0)
  append_to_start(node(x), ll)
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
  cat(string)

  invisible(x)
}

`+` <- new_external_generic("base", "+", c("e1", "e2"))
# TODO: write this
method(`+`, list(big_num, big_num)) <- function(e1, e2) {
  sum <- big_num()
}

method(`+`, list(big_num, class_numeric)) <- function(e1, e2) {
  e1 + big_num(e2)
}

method(`+`, list(class_numeric, big_num)) <- function(e1, e2) {
  big_num(e1) + e2
}

# x <- big_num("")
# x@ll
# len <- x@ll@length
# stack <- character(len)
# current <- x@ll@head

# for (i in len:1) {
#   stack[i] <- current@VALUE
#   current <- current@nxt
# }

# string <- paste0(stack, collapse = "")
# cat(string)
