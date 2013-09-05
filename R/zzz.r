.onAttach <- function(libname, pkgname) {
  packageStartupMessage("The dplyr API is currently rapidly evolving. ",
    "I recommend that you don't rely on this for production, but ",
    "feel free to explore. If you encounter a clear bug, please file a ",
    "minimal reproducible example at https://github.com/hadley/dplyr/issues.",
    "For questions and other discussion, please use ", 
    "https://groups.google.com/group/manipulatr")

  op <- options()
  op.dplyr <- list(
    dplyr.show_sql = FALSE,
    dplyr.explain_sql = FALSE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if(any(toset)) options(op.dplyr[toset])

  invisible()
}
