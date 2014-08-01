#' Select top n rows (by value).
#'
#' This is a convenient wrapper that uses \code{\link{filter}} and
#' \code{\link{min_rank}} to select the top n entries in each group, ordered
#' by \code{wt}.
#'
#' @param x a \code{\link{tbl}} to filter
#' @param n number of rows to return. If \code{x} is grouped, this is
#'   the number of rows per group. May include more than \code{n} if there
#'   are ties.
#' @param wt (Optional). The variable to use for ordering. If not specified,
#'   defaults to the last variable in the tbl.
#' @export
#' @examples
#' # Find 10 players with most games
#' data("Batting", package = "Lahman")
#' players <- group_by(tbl_df(Batting), playerID)
#' games <- tally(players, G)
#' top_n(games, 10, n)
#'
#' # A little nicer with %>%
#' tbl_df(Batting) %>%
#'   group_by(playerID) %>%
#'   tally(G) %>%
#'   top_n(10)
#'
#' # Find year with most games for each player
#' tbl_df(Batting) %>% group_by(playerID) %>% top_n(1, G)
top_n <- function(x, n, wt) {
  if (missing(wt)) {
    vars <- tbl_vars(x)
    message("Selecting by ", vars[length(vars)])
    wt <- as.name(vars[length(vars)])
  }

  call <- substitute(filter(x, min_rank(desc(wt)) <= n),
    list(n = n, wt = substitute(wt)))

  eval(call)
}
