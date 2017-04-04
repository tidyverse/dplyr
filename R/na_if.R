#' Convert values to NA
#'
#' This is a translation of the SQL command `NULL_IF`. It is useful
#' if you want to convert an annoying value to `NA`.
#'
#' @param x Vector to modify
#' @param y Value to replace with NA
#' @return A modified version of `x` that replaces any values that
#'   are equal to `y` with NA.
#' @seealso [coalesce()] to replace missing values with a specified
#'   value.
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
na_if <- function(x, y) {
  check_length(y, x, fmt_args("y"), glue("same as {fmt_args(~x)}"))

  x[x == y] <- NA
  x
}
