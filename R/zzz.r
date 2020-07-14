
# FIXME: Temporary util until the API change from
# https://github.com/r-lib/vctrs/pull/1155 has been on CRAN for long
# enough to reasonably expect that users have upgraded
dplyr_proxy_order <- function(x) NULL

# Hack to pass CRAN check with vctrs 0.3.1,
# where `vec_proxy_order()` doesn't exist
utils::globalVariables("vec_proxy_order")

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dplyr <- list(
    dplyr.show_progress = TRUE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if (any(toset)) options(op.dplyr[toset])

  if ("relax" %in% names(formals(vec_proxy_compare))) {
    dplyr_proxy_order <<- function(x, ..., relax = TRUE) {
      vec_proxy_compare(x, ..., relax = relax)
    }
  } else {
    dplyr_proxy_order <<- vec_proxy_order
  }

  .Call(dplyr_init_library, ns_env("dplyr"))

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
