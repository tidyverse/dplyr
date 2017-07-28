.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dplyr <- list(
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
