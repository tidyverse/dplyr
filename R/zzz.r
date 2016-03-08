.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dplyr <- list(
    dplyr.strict_sql = FALSE,
    dplyr.print_min = 10L,
    dplyr.print_max = 20L,
    dplyr.show_progress = TRUE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if(any(toset)) options(op.dplyr[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  when_attached("data.table", {
    if (!is_attached("dtplyr")) {
      packageStartupMessage(rule())
      packageStartupMessage(
        "data.table + dplyr code now lives in dtplyr.\n",
        "Please library(dtplyr)!"
      )
      packageStartupMessage(rule())
    }
  })

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
