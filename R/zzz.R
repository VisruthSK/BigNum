# As the website states, S7 uses this instead of export in NAMESPACE to register methods.
# https://rconsortium.github.io/S7/articles/packages.html
.onLoad <- function(...) {
  S7::S7_on_load()
}

.onUnload <- function(...) {
  S7::S7_on_unload()
}

S7::S7_on_build()
