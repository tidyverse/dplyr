#' Select top (or bottom) n rows (by value).
#'
#' This is a convenient wrapper that uses \code{\link{filter}} and
#' \code{\link{min_rank}} to select the top or bottom entries in each group,
#' ordered by \code{wt}.
#'
#' @param x a \code{\link{tbl}} to filter
#' @param n number of rows to return. If \code{x} is grouped, this is the
#'   number of rows per group. Will include more than \code{n} rows if
#'   there are ties.
#'
#'   If \code{n} is positive, selects the top \code{n} rows. If negative,
#'   selects the bottom \code{n} rows.
#' @param wt (Optional). The variable to use for ordering. If not specified,
#'   defaults to the last variable in the tbl.
#' @export
#' @examples
#' df <- data.frame(x = c(10, 4, 1, 6, 3, 1, 1))
#' df %>% top_n(2)
#'
#' # Negative values select bottom from group. Note that we get more
#' # than 2 values here because there's a tie: top_n() either takes
#' # all rows with a value, or none.
#' df %>% top_n(-2)
#'
#' if (require("Lahman")) {
#' # Find 10 players with most games
#' # A little nicer with %>%
#' tbl_df(Batting) %>%
#'   group_by(playerID) %>%
#'   tally(G) %>%
#'   top_n(10)
#'
#' # Find year with most games for each player
#' tbl_df(Batting) %>% group_by(playerID) %>% top_n(1, G)
#' }
top_n <- function(x, n, wt) {
  if (missing(wt)) {
    vars <- tbl_vars(x)
    message("Selecting by ", vars[length(vars)])
    wt <- as.name(vars[length(vars)])
  } else {
    wt <- substitute(wt)
  }

  stopifnot(is.numeric(n), length(n) == 1)
  if (n > 0) {
    call <- substitute(filter(x, min_rank(desc(wt)) <= n),
      list(n = n, wt = wt))
  } else {
    call <- substitute(filter(x, min_rank(wt) <= n),
      list(n = abs(n), wt = wt))
  }

  eval(call)
}
