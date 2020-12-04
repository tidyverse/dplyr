.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dplyr <- list(
    dplyr.show_progress = TRUE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if (any(toset)) options(op.dplyr[toset])

  .Call(dplyr_init_library, ns_env("dplyr"), ns_env("vctrs"), ns_env("rlang"))

  if (is_installed("dbplyr")){
    ns_dbplyr <- ns_env("dbplyr")
    if (!exists("count.tbl_sql", ns_dbplyr)) {
      s3_register("count", "tbl_sql")
    }
    if (!exists("tally.tbl_sql", ns_dbplyr)) {
      s3_register("tally", "tbl_sql")
    }
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
