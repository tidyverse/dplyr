#' Select top n rows (by value).
#'
#' This is a convenient wrapper that uses \code{\link{filter}} and
#' \code{\link{rank}} to select the top n entries in each group, ordered
#' by \code{wt}.
#'
#' @param x a \code{\link{tbl}} to filter
#' @param n number of rows to return. If \code{x} is grouped, this is
#'   the number of rows per group. May include more than \code{n} if there
#'   are ties.
#' @param wt the variable to use for ordering. If not specified, defaults to
#'   the last variable in the tbl.
#' @export
#' @examples
#' data("Batting", package = "Lahman")
#' players <- group_by(tbl_df(Batting), playerID)
#' games <- tally(players, G)
#' top_n(games, 10, n)
#'
#' # A little nicer with %>%
#' tbl_df(Batting) %>% group_by(playerID) %>% tally(G) %>% top_n(10)
top_n <- function(x, n, wt = NULL) {
  if (missing(wt)) {
    vars <- tbl_vars(x)
    message("Selecting by ", vars[length(vars)])
    wt <- as.name(vars[length(vars)])
  }

  call <- substitute(filter(x, rank(desc(wt), ties.method = "min") <= n),
    list(n = n, wt = substitute(wt)))

  eval(call)
}
