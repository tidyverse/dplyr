#' Elementwise `any()` and `all()`
#'
#' @description
#' These functions are variants of [any()] and [all()] that work elementwise
#' across multiple inputs. You can also think of these functions as generalizing
#' [`|`] and [`&`] to any number of inputs, rather than just two, for example:
#'
#' - `when_any(x, y, z)` is equivalent to `x | y | z`.
#'
#' - `when_all(x, y, z)` is equivalent to `x & y & z`.
#'
#' `when_any()` is particularly useful within [filter()] and [filter_out()] to
#' specify comma separated conditions combined with `|` rather than `&`.
#'
#' @details
#' `when_any()` and `when_all()` are "parallel" versions of [any()] and [all()]
#' in the same way that [pmin()] and [pmax()] are "parallel" versions of [min()]
#' and [max()].
#'
#' @param ... Logical vectors of equal size.
#'
#' @param na_rm Missing value handling:
#'
#'   - If `FALSE`, missing values are propagated according to the same rules as
#'     `|` and `&`.
#'
#'   - If `TRUE`, missing values are removed from the elementwise computation.
#'
#' @param size An optional output size. Only useful to specify if it is possible
#'   for `...` to be empty, with no inputs provided.
#'
#' @name when-any-all
#'
#' @seealso [base::any()], [base::all()], [cumany()], [cumall()],
#'   [base::pmin()], [base::pmax()]
#'
#' @examples
#' x <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)
#' y <- c(TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA)
#'
#' # `any()` and `all()` summarise down to 1 value
#' any(x, y)
#' all(x, y)
#'
#' # `when_any()` and `when_all()` work element by element across all inputs
#' # at the same time. Their defaults are equivalent to calling `|` or `&`.
#' when_any(x, y)
#' x | y
#'
#' when_all(x, y)
#' x & y
#'
#' # `na_rm = TRUE` is useful when you'd like to force these functions to
#' # return only `TRUE` or `FALSE`. This argument does so by removing any `NA`
#' # from the elementwise computation entirely.
#' tibble(
#'   x = x,
#'   y = y,
#'   any_propagate = when_any(x, y),
#'   any_remove = when_any(x, y, na_rm = TRUE),
#'   all_propagate = when_all(x, y),
#'   all_remove = when_all(x, y, na_rm = TRUE)
#' )
#'
#' # ---------------------------------------------------------------------------
#' # With `filter()` and `filter_out()`
#'
#' # `when_any()` is particularly useful inside of `filter()` and
#' # `filter_out()` as a way to combine comma separated conditions with `|`
#' # instead of with `&`.
#'
#' countries <- tibble(
#'   name = c("US", "CA", "PR", "RU", "US", NA, "CA", "PR", "RU"),
#'   score = c(200, 100, 150, NA, 50, 100, 300, 250, 120)
#' )
#' countries
#'
#' # Find rows where any of the following are true:
#' # - "US" and "CA" have a score between 200-300
#' # - "PR" and "RU" have a score between 100-200
#' countries |>
#'   filter(
#'     (name %in% c("US", "CA") & between(score, 200, 300)) |
#'       (name %in% c("PR", "RU") & between(score, 100, 200))
#'   )
#'
#' # With `when_any()`, you drop the explicit `|`, the extra `()`, and your
#' # conditions are all indented to the same level
#' countries |>
#'   filter(when_any(
#'     name %in% c("US", "CA") & between(score, 200, 300),
#'     name %in% c("PR", "RU") & between(score, 100, 200)
#'   ))
#'
#' # To drop these rows instead, use `filter_out()`
#' countries |>
#'   filter_out(when_any(
#'     name %in% c("US", "CA") & between(score, 200, 300),
#'     name %in% c("PR", "RU") & between(score, 100, 200)
#'   ))
#'
#' # ---------------------------------------------------------------------------
#' # Programming with `when_any()` and `when_all()`
#'
#' # The `size` argument is useful for making these functions size stable when
#' # you aren't sure how many inputs you're going to receive
#' size <- length(x)
#'
#' # Two inputs
#' inputs <- list(x, y)
#' when_all(!!!inputs, size = size)
#'
#' # One input
#' inputs <- list(x)
#' when_all(!!!inputs, size = size)
#'
#' # Zero inputs (without `size`, this would return `logical()`)
#' inputs <- list()
#' when_all(!!!inputs, size = size)
#'
#' # When no inputs are provided, these functions are consistent with `any()`
#' # and `all()`
#' any()
#' when_any(size = 1)
#'
#' all()
#' when_all(size = 1)
NULL

#' @rdname when-any-all
#' @export
when_any <- function(..., na_rm = FALSE, size = NULL) {
  check_dots_unnamed()

  check_bool(na_rm)
  missing <- if (na_rm) FALSE else NA

  vec_pany(
    ...,
    .missing = missing,
    .size = size,
    .arg = "",
    .error_call = current_env()
  )
}

#' @rdname when-any-all
#' @export
when_all <- function(..., na_rm = FALSE, size = NULL) {
  check_dots_unnamed()

  check_bool(na_rm)
  missing <- if (na_rm) TRUE else NA

  vec_pall(
    ...,
    .missing = missing,
    .size = size,
    .arg = "",
    .error_call = current_env()
  )
}
