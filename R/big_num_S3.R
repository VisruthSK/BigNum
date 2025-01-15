node_or_na_s3 <- function(value) inherits(value, "big_num_node_s3") || all(is.na(value))

#' @export
node_s3 <- function(VALUE, nxt = NA) {
  stopifnot(node_or_na_s3(nxt), is.numeric(VALUE), length(VALUE) == 1)

  structure(
    list(
      VALUE = VALUE,
      state = rlang::new_environment(list(nxt = nxt))
    ),
    class = "big_num_node_s3"
  )
}

#' @export
linked_list_s3 <- function(num = NULL) {
  force(num)

  if (is.null(num) || num == "") {
    return(structure(list(state = rlang::new_environment(list(head = NA, tail = NA, length = 0))), class = "big_num_linked_list_s3"))
  }

  extract_digit_to_node <- function(pos) node_s3(as.integer(substring(num, pos, pos))) # TODO: throw error if the char isn't numeric

  len <- nchar(num)
  nodes <- lapply(len:1, function(i) extract_digit_to_node(i))

  if (len != 1) {
    for (i in (len - 1):1) {
      nodes[[i]]$state$nxt <- nodes[[i + 1]]
    }
  }

  state <- rlang::new_environment(list(
    head = nodes[[1]],
    tail = nodes[[len]],
    length = len
  ))

  structure(
    list(
      state = state
    ),
    class = "big_num_linked_list_s3"
  )
}

#' @export
big_num_s3 <- function(num = "") {
  structure(linked_list_s3(num), class = c("big_num_s3", "big_num_linked_list_s3"))
}

#' @export
is_even_s3 <- function(x) {
  UseMethod("is_even_s3")
}
#' @export
is_even_s3.big_num_s3 <- function(x) {
  x$state$head$VALUE %% 2 == 0
}

#' @export
bn_append_s3 <- function(x, ll) {
  UseMethod("bn_append_s3")
}

#' @export
bn_append_s3.big_num_node_s3 <- function(x, ll) {
  if (!inherits(ll$state$head, "big_num_node_s3")) {
    ll$state$head <- x
    ll$state$tail <- x
  } else {
    ll$state$tail$state$nxt <- x
    ll$state$tail <- x
  }
  ll$state$length <- ll$state$length + 1

  invisible(ll)
}

#' @exportS3Method base::print
print.big_num_linked_list_s3 <- function(x, ...) {
  current <- x$state$head
  while (inherits(current, "big_num_node_s3")) {
    cat(current$VALUE, "-> ")
    current <- current$state$nxt
  }
  cat("NA\n")
  
  invisible(x)
}

#' @exportS3Method base::print
print.big_num_s3 <- function(x, ...) {
  len <- x$state$length

  if (len == 0) {
    return(cat("NA\n"))
  }

  stack <- character(len)
  current <- x$state$head

  for (i in len:1) {
    stack[i] <- current$VALUE
    current <- current$state$nxt
  }

  string <- paste0(stack, collapse = "")
  cat(string, "\n")
}
