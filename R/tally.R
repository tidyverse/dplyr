#' Counts/tally observations by group.
#'
#' \code{tally} is a convenient wrapper for summarise that will either call
#' \code{\link{n}} or \code{\link{sum}(n)} depending on whether you're tallying
#' for the first time, or re-tallying. \code{count()} is similar, but also
#' does the \code{\link{group_by}} for you.
#'
#' @param x a \code{\link{tbl}} to tally/count.
#' @param ...,vars Variables to group by.
#' @param wt (Optional) If omitted, will count the number of rows. If specified,
#'   will perform a "weighted" tally by summing the (non-missing) values of
#'   variable \code{wt}.
#' @param sort if \code{TRUE} will sort output in descending order of \code{n}
#' @export
#' @examples
#' if (require("Lahman")) {
#' batting_tbl <- tbl_df(Batting)
#' tally(group_by(batting_tbl, yearID))
#' tally(group_by(batting_tbl, yearID), sort = TRUE)
#'
#' # Multiple tallys progressively roll up the groups
#' plays_by_year <- tally(group_by(batting_tbl, playerID, stint), sort = TRUE)
#' tally(plays_by_year, sort = TRUE)
#' tally(tally(plays_by_year))
#'
#' # This looks a little nicer if you use the infix %>% operator
#' batting_tbl %>% group_by(playerID) %>% tally(sort = TRUE)
#'
#' # count is even more succinct - it also does the grouping for you
#' batting_tbl %>% count(playerID)
#' batting_tbl %>% count(playerID, wt = G)
#' batting_tbl %>% count(playerID, wt = G, sort = TRUE)
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

  tally_(x, wt, sort = sort)
}

tally_ <- function(x, wt, sort = FALSE) {
  if (is.null(wt)) {
    n <- quote(n())
  } else {
    n <- lazyeval::interp(quote(sum(wt, na.rm = TRUE)), wt = wt)
  }

  out <- summarise_(x, n = n)

  if (!sort) {
    out
  } else {
    arrange(out, desc(n))
  }
}

#' @export
#' @rdname tally
count <- function(x, ..., wt = NULL, sort = FALSE) {
  vars <- lazyeval::lazy_dots(...)
  wt <- substitute(wt)

  count_(x, vars, wt, sort = sort)
}

#' @export
#' @rdname tally
count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  old_vars <- lapply(groups(x), lazyeval::as.lazy, env=parent.frame())
  grouped <- group_by_(x, .dots = c(old_vars, vars))
  tally_(grouped, wt = wt, sort = sort)
}
