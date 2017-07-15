#' Select top (or bottom) n rows (by value).
#'
#' This is a convenient wrapper that uses [filter()] and
#' [min_rank()] to select the top or bottom entries in each group,
#' ordered by `wt`.
#'
#' @param x a [tbl()] to filter
#' @param n number of rows to return. If `x` is grouped, this is the
#'   number of rows per group. Will include more than `n` rows if
#'   there are ties.
#'
#'   If `n` is positive, selects the top `n` rows. If negative,
#'   selects the bottom `n` rows.
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

  stopifnot(is.numeric(n), length(n) == 1, is.name(wt))
  if (n > 0) {
    call <- substitute(
      filter(x, min_rank(desc(wt)) <= n),
      list(n = n, wt = wt)
    )
  } else {
    call <- substitute(
      filter(x, min_rank(wt) <= n),
      list(n = abs(n), wt = wt)
    )
  }

  eval(call)
}
