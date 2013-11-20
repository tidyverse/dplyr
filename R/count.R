#' Count observations by group.
#' 
#' \code{count} is a convenient wrapper for summarise that either call 
#' \code{\link{n}} or \code{\link{sum}(n)} depending on whether you're counting 
#' for the first time, or recounting. 
#' 
#' @param a \code{\link{tbl}} to count
#' @param wt if not specified, will count the number of rows. If specified,
#'   will perform a "weighted" count but summing over the specified variable.
#' @export
#' @examples
#' if (require("Lahman")) {
#'   batting_tbl <- tbl_cpp(Batting)
#'   count(group_by(batting_tbl, yearID))
#'   
#'   plays_by_year <- count(group_by(batting_tbl, playerID, stint))
#'   count(plays_by_year)
#'   count(count(plays_by_year))
#'   count(group_by(plays_by_year, stint))
#'   
#'   # This looks a little nicer if you use the infix %.% operator
#'   batting_tbl %.% group_by(playerID) %.% count()
#' }
count <- function(x, wt) {  
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

#' Select top n rows (by value).
#' 
#' This is a convenient wrapper that uses \code{\link{filter}} and 
#' \code{\link{rank}} to select the top n entries in each group, ordered
#' by \code{wt}.
#' 
#' @param a \code{\link{tbl}} to filter
#' @param n number of rows to return. If \code{x} is grouped, this is
#'   the number of rows per group. May include more than \code{n} if there
#'   are ties.
#' @param wt the variable to use for ordering. If not specified, defaults to
#'   the last varible in the tbl.
#' @export
#' @examples
#' if (require("Lahman")) {
#'   players <- group_by(tbl_df(Batting), playerID)
#'   games <- count(players, G)
#'   top_n(games, 10, n)
#' 
#'   # A little nicer with %.%
#'   tbl_df(Batting) %.% group_by(playerID) %.% count(G) %.% top_n(10)
#' }
top_n <- function(x, n, wt = NULL) {
  if (is.null(wt)) {
    vars <- tbl_vars(x)
    message("Selecting by ", vars[length(vars)])
    wt <- as.name(vars[length(vars)])
  }
  
  call <- substitute(filter(x, rank(desc(wt), ties.method = "min") < n), 
    list(n = n, wt = substitute(wt)))
  
  eval(call)
}