#' Group input by rows
#'
#' `rowwise()` allows you to compute on a data frame a row-at-a-time.
#' This is most useful when a vectorised function doesn't exist.
#'
#' @section List-columns:
#' Because a rowwise has exactly one row per group it offers a small
#' convenience for working with list-columns. Normally, `summarise()` and
#' `mutate()` extract a groups worth of data with `[`. But when you index
#' a list in this way, you get back another list. When you're working with
#' a `rowwise` tibble, then dplyr will use `[[` instead of `[` to make your
#' life a little easier.
#'
#' @param data Input data frame.
#' @param ... <[`tidy-select`][dplyr_tidy_select]> Variables to be preserved
#'   when calling [summarise()]. This is typically a set of variables whose
#'   combination uniquely identify each row.
#' @export
#' @examples
#' df <- tibble(x = runif(6), y = runif(6))
#' # Compute the mean of x and y in each row
#' df %>% rowwise() %>% mutate(z = mean(c(x, y)))
#'
#' # Compute the min or x and y in each row
#' df %>% rowwise() %>% mutate(z = min(c(x, y)))
#' # In this case you can use an existing vectorised function:
#' df %>% mutate(z = pmin(x, y))
#' # Where these functions exist they'll be much faster than rowwise
#' # so be on the lookout for them.
#'
#' # rowwise() + do() is also useful when doing simulations
#' params <- tribble(
#'  ~sim, ~n, ~mean, ~sd,
#'     1,  1,     1,   1,
#'     2,  2,     2,   4,
#'     3,  3,    -1,   2
#' )
#' # Here I supply variables to preserve after the summary
#' params %>%
#'   rowwise(sim) %>%
#'   summarise(z = rnorm(n, mean, sd))
#'
#' # If you want one row per simulation, put the results in a list()
#' params %>%
#'   rowwise(sim) %>%
#'   summarise(z = list(rnorm(n, mean, sd)))
#'
#' # Or use do() which do this automatically:
#' params %>%
#'   rowwise(sim) %>%
#'   do(z = rnorm(n, mean, sd))
rowwise <- function(data, ...) {
  vars <- tidyselect::eval_select(expr(c(...)), data, include = group_vars(data))
  rowwise_df(data, vars)
}

# Constructor + helper ----------------------------------------------------

rowwise_df <- function(data, group_vars) {
  group_data <- as_tibble(data)[group_vars]
  new_rowwise_df(data, group_data)
}

new_rowwise_df <- function(data, group_data) {
  if (!is_tibble(group_data) || has_name(group_data, ".rows")) {
    abort("`group_data` must be a tibble without a `.rows` column")
  }

  group_data$.rows <- new_list_of(as.list(seq_len(nrow(data))), ptype = integer())
  new_tibble(data, groups = group_data, class = "rowwise_df")
}
setOldClass(c("rowwise_df", "tbl_df", "tbl", "data.frame"))

# methods -----------------------------------------------------------------

#' @export
tbl_sum.rowwise_df <- function(x) {
  c(
    NextMethod(),
    "Rowwise" = commas(group_vars(x))
  )
}

#' @importFrom tibble is_tibble
#' @export
`[.rowwise_df` <- function(x, i, j, drop = FALSE) {
  out <- NextMethod()

  if (!is.data.frame(out)) {
    return(out)
  }

  if (drop) {
    as_tibble(out)
  } else {
    group_vars <- intersect(names(out), group_vars(x))
    rowwise_df(out, group_vars)
  }
}

#' @export
`[<-.rowwise_df` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  group_vars <- intersect(names(out), group_vars(x))
  rowwise_df(out, group_vars)
}

#' @export
`names<-.rowwise_df` <- function(x, value) {
  data <- NextMethod()
  group_vars <- value[match(group_vars(x), names(x))]

  rowwise_df(data, group_vars)
}
