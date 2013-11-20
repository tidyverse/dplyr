#' Count.
#' 
#' Count is a convenient wrapper for summarise that either call \code{\link{n}}
#' or \code{\link{sum}(n)} depending on whether you're counting for the first
#' time, or recounting.
#' 
#' @param a \code{\link{tbl}} to count
#' @param ... variables to group by
#' @param wt if not specified, will count the number of rows. If specified,
#'   will perform a "weighted" count but summing over the specified variable.
#' @export
#' @examples
#' if (require("Lahman")) {
#'   batting_tbl <- tbl_cpp(Batting)
#'   count(batting_tbl, yearID)
#'   
#'   plays_by_year <- count(batting_tbl, playerID, stint)
#'   count(plays_by_year, playerID)
#'   count(plays_by_year, stint)
#' }
count <- function(x, ..., wt) {
  grouped <- group_by(ungroup(x), ...)
  
  if (missing(wt)) {
    if ("n" %in% names(x)) {
      message("Using n as weighting variable")
      wt <- quote(n)
    } else {
      wt <- NULL
    }
  }

  if (is.null(wt)) {
    summarise(grouped, n = n())  
  } else {
    wt <- substitute(summarise(grouped, n = sum(wt)), list(wt = wt))
    eval(wt)
  }
}  