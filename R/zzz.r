Rcpp_version <- utils::packageVersion("Rcpp")

.onLoad <- function(libname, pkgname) {
  if (Rcpp_version != utils::packageVersion("Rcpp")) {
    warning(
      "Installed Rcpp (", utils::packageVersion("Rcpp"), ") different from ",
      "Rcpp used to build dplyr (", Rcpp_version, ").\n",
      "Please reinstall dplyr to avoid random crashes.",
      call. = FALSE
    )
  }

  op <- options()
  op.dplyr <- list(
    dplyr.strict_sql = FALSE,
    dplyr.show_progress = TRUE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if (any(toset)) options(op.dplyr[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("plyr", "attach"), function(...) {
    packageStartupMessage(rule())
    packageStartupMessage("You have loaded plyr after dplyr - this is likely ",
      "to cause problems.\nIf you need functions from both plyr and dplyr, ",
      "please load plyr first, then dplyr:\nlibrary(plyr); library(dplyr)")
    packageStartupMessage(rule())
  })
}

when_attached <- function(pkg, action) {
  if (is_attached(pkg)) {
    action
  } else {
    setHook(packageEvent(pkg, "attach"), function(...) action)
  }
}

is_attached <- function(pkg) paste0("package:", pkg) %in% search()
