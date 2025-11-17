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
#'
#' @seealso
#'
#'   - [coalesce()] to replace `NA`s with the first non-missing value.
#'
#'   - [replace_values()] for making arbitrary replacements by value.
#'
#'   - [replace_when()] for making arbitrary replacements using logical
#'     conditions.
#'
#' @export
#' @examples
#' # `na_if()` is useful for replacing a single problematic value with `NA`
#' na_if(c(-99, 1, 4, 3, -99, 5), -99)
#' na_if(c("abc", "def", "", "ghi"), "")
#'
#' # You can use it to standardize `NaN`s to `NA`
#' na_if(c(1, NaN, NA, 2, NaN), NaN)
#'
#' # Because `na_if()` is an R translation of SQL's `NULLIF` command,
#' # it compares `x` and `y` element by element. Where `x` and `y` are
#' # equal, the value in `x` is replaced with an `NA`.
#' na_if(
#'   x = c(1, 2, 5, 5, 6),
#'   y = c(0, 2, 3, 5, 4)
#' )
#'
#' # If you have multiple problematic values that you'd like to replace with
#' # `NA`, then `replace_values()` is a better choice than `na_if()`
#' x <- c(-99, 1, 4, 0, -99, 5, -1, 0, 5)
#' replace_values(x, c(0, -1, -99) ~ NA)
#'
#' # You'd have to nest `na_if()`s to achieve this
#' try(na_if(x, c(0, -1, -99)))
#' na_if(na_if(na_if(x, 0), -1), -99)
#'
#' # If you'd like to replace values that match a logical condition with `NA`,
#' # use `replace_when()`
#' replace_when(x, x < 0 ~ NA)
#'
#' # If you'd like to replace `NA` with some other value, use `replace_values()`
#' x <- c(NA, 5, 2, NA, 0, 3)
#' replace_values(x, NA ~ 0)
#'
#' # `na_if()` is particularly useful inside `mutate()`
#' starwars |>
#'   select(name, eye_color) |>
#'   mutate(eye_color = na_if(eye_color, "unknown"))
#'
#' # `na_if()` can also be used with `mutate()` and `across()`
#' # to alter multiple columns
#' starwars |>
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
