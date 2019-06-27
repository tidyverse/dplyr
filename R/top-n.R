#' Select top (or bottom) n rows (by value)
#'
#' This is a convenient wrapper that uses [filter()] and
#' [min_rank()] to select the top or bottom entries in each group,
#' ordered by `wt`.
#'
#' @param x a [tbl()] to filter
#' @param n number of rows to return for `top_n()`, fraction of rows to
#'   return for `top_frac()`.
#'
#'   If `x` is grouped, this is the
#'   number (or fraction) of rows per group. Will include more rows if
#'   there are ties.
#'
#'   If `n` is positive, selects the top rows. If negative,
#'   selects the bottom rows.
#'
#' @param wt (Optional). The variable to use for ordering. If not
#'   specified, defaults to the last variable in the tbl.
#'
#' @details
#'   Both `n` and `wt` are automatically [quoted][rlang::enquo] and later
#'   [evaluated][rlang::eval_tidy] in the context of the data
#'   frame. It supports [unquoting][rlang::quasiquotation].
#'
#' @export
#' @examples
#' df <- data.frame(x = c(10, 4, 1, 6, 3, 1, 1))
#' df %>% top_n(2)
#'
#' # half the rows
#' df %>% top_n(n() * .5)
#' df %>% top_frac(.5)
#'
#' # Negative values select bottom from group. Note that we get more
#' # than 2 values here because there's a tie: top_n() either takes
#' # all rows with a value, or none.
#' df %>% top_n(-2)
#'
#' if (require("Lahman")) {
#' # Find 10 players with most games
#' tbl_df(Batting) %>%
#'   group_by(playerID) %>%
#'   tally(G) %>%
#'   top_n(10)
#'
#' # Find year with most games for each player
#' \dontrun{
#' tbl_df(Batting) %>%
#'   group_by(playerID) %>%
#'   top_n(1, G)
#' }
#' }
top_n <- function(x, n, wt) {
  nn <- enquo(n)
  wt <- enquo(wt)

  if (quo_is_missing(wt)) {
    vars <- tbl_vars(x)
    wt_name <- vars[length(vars)]
    inform(glue("Selecting by ", wt_name))
    wt <- sym(wt_name)
  }

  pred <- expr(local({
    .n <- !!nn
    if (.n > 0) {
      min_rank(desc(!!wt)) <= .n
    } else {
      min_rank(!!wt) <= abs(.n)
    }
  }))
  filter(x, !!pred)
}

#' @export
#' @rdname top_n
top_frac <- function(x, n, wt) {
  top_n(x, !!enquo(n) * n(), !!enquo(wt))
}
