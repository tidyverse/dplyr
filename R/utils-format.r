#' Describing dimensions
#'
#' Prints the dimensions of an array-like object in a user-friendly manner,
#' substituting `NA` with ?? (for SQL queries).
#'
#' @param x Object to show dimensions for.
#' @export
#' @keywords internal
#' @examples
#' dim_desc(mtcars)
dim_desc <- function(x) {
  d <- dim(x)
  d2 <- big_mark(d)
  d2[is.na(d)] <- "??"

  paste0("[", paste0(d2, collapse = " x "), "]")
}

# function for the thousand separator,
# returns "," unless it's used for the decimal point, in which case returns "."
big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

rule <- function(pad = "-", gap = 2L) {
  paste0(rep(pad, getOption("width") - gap), collapse = "")
}
