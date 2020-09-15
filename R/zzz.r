
# Initialised at load time
dplyr_proxy_order <- function(...) NULL

# Hack to pass CRAN check with older vctrs versions where
# `vec_proxy_order()` doesn't exist
utils::globalVariables("vec_proxy_order")


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dplyr <- list(
    dplyr.show_progress = TRUE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if (any(toset)) options(op.dplyr[toset])

  .Call(dplyr_init_library, ns_env("dplyr"), ns_env("vctrs"), ns_env("funs"))

  # FIXME: Temporary until the API change from
  # https://github.com/r-lib/vctrs/pull/1155 is on CRAN and we can
  # depend on it
  if (env_has(ns_env("vctrs"), "vec_proxy_order")) {
    dplyr_proxy_order <<- vec_proxy_order
  } else {
    dplyr_proxy_order <<- function(x, ...) vec_proxy_compare(x, ..., relax = TRUE)
  }

  invisible()
}

.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("plyr", "attach"), function(...) {
    packageStartupMessage(rule())
    packageStartupMessage(
      "You have loaded plyr after dplyr - this is likely ",
      "to cause problems.\nIf you need functions from both plyr and dplyr, ",
      "please load plyr first, then dplyr:\nlibrary(plyr); library(dplyr)"
    )
    packageStartupMessage(rule())
  })
}

.onDetach <- function(libpath) {
  setHook(packageEvent("plyr", "attach"), NULL, "replace")
}
