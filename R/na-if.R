#' Convert values to `NA`
#'
#' This is a translation of the SQL command `NULLIF`. It is useful if you want
#' to convert an annoying value to `NA`.
#'
#' @param x Vector to modify
#' @param y Value or vector to compare against. When `x` and `y` are equal, the
#'   value in `x` will be replaced with `NA`.
#'
#'   `y` is [cast][vctrs::theory-faq-coercion] to the type of `x` before
#'   comparison.
#'
#'   `y` is [recycled][vctrs::theory-faq-recycling] to the size of `x` before
#'   comparison. This means that `y` can be a vector with the same size as `x`,
#'   but most of the time this will be a single value.
#' @return A modified version of `x` that replaces any values that
#'   are equal to `y` with `NA`.
#' @seealso [coalesce()] to replace missing values with a specified
#'   value.
#'
#'   [tidyr::replace_na()] to replace `NA` with a value.
#' @export
#' @examples
#' na_if(1:5, 5:1)
#'
#' x <- c(1, -1, 0, 10)
#' 100 / x
#' 100 / na_if(x, 0)
#'
#' y <- c("abc", "def", "", "ghi")
#' na_if(y, "")
#'
#' # `na_if()` allows you to replace `NaN` with `NA`,
#' # even though `NaN == NaN` returns `NA`
#' z <- c(1, NaN, NA, 2, NaN)
#' na_if(z, NaN)
#'
#' # `na_if()` is particularly useful inside `mutate()`,
#' # and is meant for use with vectors rather than entire data frames
#' starwars %>%
#'   select(name, eye_color) %>%
#'   mutate(eye_color = na_if(eye_color, "unknown"))
#'
#' # `na_if()` can also be used with `mutate()` and `across()`
#' # to alter multiple columns
#' starwars %>%
#'    mutate(across(where(is.character), ~na_if(., "unknown")))
na_if <- function(x, y) {
  # Type and size stable on `x`
  y <- vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
  y <- vec_recycle(y, size = vec_size(x), x_arg = "y")

  na <- vec_init(x)
  where <- vec_equal(x, y, na_equal = TRUE)

  x <- vec_assign(x, where, na)

  x
}
