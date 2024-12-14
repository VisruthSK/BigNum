node_or_na_s4 <- function(value) is(value, "big_num_node_s4") || is.na(value)

setClass("big_num_node_s4",
  slots = c(
    VALUE = "numeric",
    state = "environment"
  ),
  prototype = list(
    VALUE = NA_real_
  )
)

node_s4 <- function(VALUE, nxt = NA) {
  stopifnot(node_or_na_s4(nxt), is.numeric(VALUE), length(VALUE) == 1)

  new("big_num_node_s4", VALUE = VALUE, state = rlang::new_environment(list(nxt = nxt)))
}

# TODO: copy this in S3 to approximate read only properties?
setGeneric("nxt", function(x) standardGeneric("nxt"))
setMethod("nxt", "big_num_node_s4", function(x) x@state$nxt)

setGeneric("nxt<-", function(x, value) standardGeneric("nxt<-"))
setMethod("nxt<-", "big_num_node_s4", function(x, value) {
  x@state$nxt <- value
  x
})

setGeneric("VALUE", function(x) standardGeneric("VALUE"))
setMethod("VALUE", "big_num_node_s4", function(x) x@VALUE)

setGeneric("VALUE<-", function(x, value) standardGeneric("VALUE<-"))
setMethod("VALUE<-", "big_num_node_s4", function(x, value) {
  stop("Can't set read-only property VALUE ")
})

setClass("big_num_linked_list_s4",
  slots = c(
    state = "environment"
  )
)

linked_list_s4 <- function(num = NULL) {
  force(num)

  if (is.null(num) || num == "") {
    return(new("big_num_linked_list_s4", state = rlang::new_environment(list(head = NA, tail = NA, length = 0))))
  }

  extract_digit_to_node <- function(pos) node_s4(as.integer(substring(num, pos, pos)))

  len <- nchar(num)
  nodes <- lapply(len:1, function(i) extract_digit_to_node(i))

  if (len != 1) {
    for (i in (len - 1):1) {
      nodes[[i]]@state$nxt <- nodes[[i + 1]]
    }
  }

  state <- rlang::new_environment(list(
    head = nodes[[1]],
    tail = nodes[[len]],
    length = len
  ))

  new("big_num_linked_list_s4", state = state)
}

setClass("big_num_s4", contains = "big_num_linked_list_s4")

big_num_s4 <- function(num = "") {
  as(linked_list_s4(num), "big_num_s4")
}

setMethod("show", "big_num_linked_list_s4", function(object) {
  current <- object@state$head
  while (is(current, "big_num_node_s4")) {
    cat(current@VALUE, "-> ")
    current <- current@state$nxt
  }
  cat("NA\n")
})

setMethod("show", "big_num_s4", function(object) {
  len <- object@state$length

  if (len == 0) {
    return(cat("NA\n"))
  }

  stack <- character(len)
  current <- object@state$head

  for (i in len:1) {
    stack[i] <- current@VALUE
    current <- current@state$nxt
  }

  string <- paste0(stack, collapse = "")
  cat(string, "\n")
})
