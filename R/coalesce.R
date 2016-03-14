#' Find first non-missing element
#'
#' Given a set of vectors, \code{coelesce} finds the first non-missing value
#' at each position. This is inspired by the SQL \code{COALESCE} function
#' which does the same thing for \code{NULL}s.
#'
#' @param x,... Vectors. All inputs should either be length 1, or the
#'   same length as \code{x}
#' @return A vector the same length as \code{x} with missing values replaced
#'   by the first non-missing value.
#' @seealso \code{\link{na_if}()} to replace specified values with a \code{NA}.
#' @export
#' @examples
#' # Use a single value to replace all missing values
#' x <- sample(c(1:5, NA, NA, NA))
#' coalesce(x, 0)
#'
#' # Or match together a complete vector from missing pieces
#' y <- c(1, 2, NA, NA, 5)
#' z <- c(NA, NA, 3, 4, 5)
#' coalesce(y, z)
coalesce <- function(x, ...) {
  n <- length(x)

  values <- list(...)
  for (i in seq_along(values)) {
    val <- values[[i]]
    val_n <- length(val)

    if (val_n == 1L) {
      x[is.na(x)] <- val
    } else if (val_n == n) {
      x[is.na(x)] <- val[is.na(x)]
    } else {
      stop("Vector at position ", i, " is not length 1 or ", n, call. = FALSE)
    }
  }
  x
}
