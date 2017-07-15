#' Select top (or bottom) n rows (by value)
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
#' @param wt (Optional). The variable to use for ordering. If not
#'   specified, defaults to the last variable in the tbl.
#'
#'   This argument is automatically [quoted][rlang::quo] and later
#'   [evaluated][rlang::eval_tidy] in the context of the data
#'   frame. It supports [unquoting][rlang::quasiquotation]. See
#'   `vignette("programming")` for an introduction to these concepts.
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
  wt <- enquo(wt)

  if (quo_is_missing(wt)) {
    vars <- tbl_vars(x)
    wt_name <- vars[length(vars)]
    inform(glue("Selecting by ", wt_name))
    wt <- sym(wt_name)
  }

  if (!is_scalar_integerish(n)) {
    abort("`n` must be a scalar integer")
  }

  if (n > 0) {
    quo <- quo(filter(x, min_rank(desc(!! wt)) <= !! n))
  } else {
    quo <- quo(filter(x, min_rank(!! wt) <= !! abs(n)))
  }

  eval_tidy(quo)
}
