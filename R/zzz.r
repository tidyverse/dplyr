.onAttach <- function(libname, pkgname) {
  op <- options()
  op.dplyr <- list(
    dplyr.show_sql = FALSE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if(any(toset)) options(op.dplyr[toset])

  invisible()
}
