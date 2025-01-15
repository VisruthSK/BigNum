# As the website states, S7 uses this instead of `@export` to register methods.
# https://rconsortium.github.io/S7/reference/methods_register.html
.onLoad <- function(lib, pkg) {
  S7::methods_register()
}

# The following is also taken from the website:
# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL
