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

  fmt_dims(d2)
}

wrap <- function(..., indent = 0) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(
    x,
    indent = indent,
    exdent = indent + 2,
    width = getOption("width")
  )

  paste0(wrapped, collapse = "\n")
}

ruler <- function(width = getOption("width")) {
  x <- seq_len(width)
  y <- case_when(
    x %% 10 == 0 ~ as.character((x %/% 10) %% 10),
    x %% 5 == 0  ~ "+",
    TRUE         ~ "-"
  )
  cat(y, "\n", sep = "")
  cat(x %% 10, "\n", sep = "")
}

rule <- function(pad = "-", gap = 2L) {
  paste0(rep(pad, getOption("width") - gap), collapse = "")
}

named_rule <- function(..., pad = "-") {
  if (nargs() == 0) {
    title <- ""
  } else {
    title <- paste0(...)
  }
  paste0(title, " ", rule(pad = pad, gap = nchar(title) - 1))
}


# function for the thousand separator,
# returns "," unless it's used for the decimal point, in which case returns "."
big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

paste_line <- function(..., .trailing = FALSE) {
  lines <- paste(chr(...), collapse = "\n")
  if (.trailing) {
    lines <- paste0(lines, "\n")
  }
  lines
}
