#' Tally observations by group.
#' 
#' \code{tally} is a convenient wrapper for summarise that either call 
#' \code{\link{n}} or \code{\link{sum}(n)} depending on whether you're tallying 
#' for the first time, or re-tallying. 
#' 
#' @param x a \code{\link{tbl}} to tally
#' @param wt if not specified, will tally the number of rows. If specified,
#'   will perform a "weighted" tally but summing over the specified variable.
#' @export
#' @examples
#' if (require("Lahman")) {
#'   batting_tbl <- tbl_cpp(Batting)
#'   tally(group_by(batting_tbl, yearID))
#'   
#'   plays_by_year <- tally(group_by(batting_tbl, playerID, stint))
#'   tally(plays_by_year)
#'   # FIXME: https://github.com/hadley/dplyr/issues/129
#'   # tally(tally(plays_by_year))
#'   tally(group_by(plays_by_year, stint))
#'   
#'   # This looks a little nicer if you use the infix %.% operator
#'   batting_tbl %.% group_by(playerID) %.% tally()
#' }
tally <- function(x, wt) {  
  if (missing(wt)) {
    if ("n" %in% names(x)) {
      message("Using n as weighting variable")
      wt <- quote(n)
    } else {
      wt <- NULL
    }
  } else {
    wt <- substitute(wt)
  }

  if (is.null(wt)) {
    summarise(x, n = n())  
  } else {
    wt <- substitute(summarise(x, n = sum(wt)), list(wt = wt))
    eval(wt)
  }
}
