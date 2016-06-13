#' Convert values to NA.
#'
#' This is a translation of the SQL command \code{NULL_IF}. It is useful
#' if you want to convert an annoying value to \code{NA}.
#'
#' @param x Vector to modify
#' @param y If th
#' @return A modified version of \code{x} that replaces any values that
#'   are equal to \code{y} with NA.
#' @seealso \code{\link{coalesce}()} to replace missing values with a specified
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
  if (length(y) != length(x) && length(y) != 1) {
    stop("`y` must be length 1 or same length as `x`", call. = FALSE)
  }

  x[x == y] <- NA
  x
}
