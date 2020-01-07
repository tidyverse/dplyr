#' Select top (or bottom) n rows (by value)
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("retired")}
#' `top_n()` has been retired in favour of [slice_min()]/[slice_max()].
#' While it will not be deprecated in the near future, retirement means
#' that we will only perform critical bug fixes, so we recommend moving to the
#' newer alternatives.
#'
#' `top_n()` was retired because the name was fundamentally confusing as
#' it returned what you might reasonably consider to be the _bottom_
#' rows. Additionally, the `wt` variable had a confusing name, and strange
#' default (the last column in the data frame). Unfortunately we could not
#' see an easy way to fix the existing `top_n()` function without breaking
#' existing code, so we created a new alternative.
#'
#' @param x A data frame.
#' @param n Number of rows to return for `top_n()`, fraction of rows to
#'   return for `top_frac()`. If `n` is positive, selects the top rows.
#'   If negative, selects the bottom rows.

#'   If `x` is grouped, this is the number (or fraction) of rows per group.
#'   Will include more rows if there are ties.
#' @param wt (Optional). The variable to use for ordering. If not
#'   specified, defaults to the last variable in the tbl.
#' @keywords internal
#' @export
#' @examples
#' df <- data.frame(x = c(6, 4, 1, 10, 3, 1, 1))
#'
#' df %>% top_n(2)  # highest values
#' df %>% top_n(-2) # lowest values
#' # now use
#' df %>% slice_max(x, n = 2)
#' df %>% slice_min(x, n = 2)
#'
#' # top_frac() -> prop argument of slice_min()/slice_max()
#' df %>% top_frac(.5)
#' # ->
#' df %>% slice_max(x, prop = 0.5)
top_n <- function(x, n, wt) {
  wt <- enquo(wt)
  if (quo_is_missing(wt)) {
    vars <- tbl_vars(x)
    wt_name <- vars[length(vars)]
    inform(glue("Selecting by ", wt_name))
    wt <- sym(wt_name)
  }

  filter(x, top_n_rank({{ n }}, !!wt))
}

top_n_rank <- function(n, wt) {
  if (n > 0) {
    min_rank(desc(wt)) <= n
  } else {
    min_rank(wt) <= abs(n)
  }
}

#' @export
#' @rdname top_n
top_frac <- function(x, n, wt) {
  top_n(x, {{ n }} * n(), {{ wt }})
}
