#' Tally observations by group.
#'
#' \code{tally} is a convenient wrapper for summarise that will either call
#' \code{\link{n}} or \code{\link{sum}(n)} depending on whether you're tallying
#' for the first time, or re-tallying.
#'
#' @param x a \code{\link{tbl}} to tally
#' @param wt (Optional) If not specified, will tally the number of rows.
#'   If specified, will perform a "weighted" tally but summing over the
#'   specified variable.
#' @param sort if \code{TRUE} will sort output in descending order of \code{n}
#' @export
#' @examples
#' if (require("Lahman")) {
#' batting_tbl <- tbl_df(Batting)
#' tally(group_by(batting_tbl, yearID))
#' tally(group_by(batting_tbl, yearID), sort = TRUE)
#'
#' # Multiple tallys progressively role up the groups
#' plays_by_year <- tally(group_by(batting_tbl, playerID, stint), sort = TRUE)
#' tally(plays_by_year, sort = TRUE)
#' tally(tally(plays_by_year))
#'
#' # This looks a little nicer if you use the infix %>% operator
#' batting_tbl %>% group_by(playerID) %>% tally(sort = TRUE)
#' }
tally <- function(x, wt, sort = FALSE) {
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
    out <- summarise(x, n = n())
  } else {
    wt <- substitute(summarise(x, n = sum(wt)), list(wt = wt))
    out <- eval(wt)
  }

  if (sort) {
    arrange(out, desc(n))
  } else {
    out
  }
}
