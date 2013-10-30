#' Tools for describing matrices
#'
#' @param x a matrix to describe
#' @param n number of rows to show
#' @keywords internal
#' @examples
#' dim_desc(mtcars)
#' trunc_mat(mtcars)
#' @name dplyr-formatting
NULL

#' @export
#' @rdname dplyr-formatting
dim_desc <- function(x) {
  d <- dim(x)
  d2 <- format(d, big.mark = ",", justify = "none", trim = TRUE)
  d2[is.na(d)] <- "??"
  
  paste0("[", paste0(d2, collapse = " x "), "]")
}

#' @export
#' @rdname dplyr-formatting
trunc_mat <- function(x, n = NULL) {
  rows <- nrow(x)
  if (!is.na(rows) && rows == 0) return()
  
  n <- n %||% if (is.na(rows) || rows > 99) 10L else rows
  
  df <- as.data.frame(head(x, n))
  mat <- format(df, justify = "left")

  width <- getOption("width")

  values <- c(format(rownames(mat))[[1]], unlist(mat[1, ]))
  names <- c("", colnames(mat))
  w <- pmax(nchar(values), nchar(names))
  cumw <- cumsum(w + 1)

  too_wide <- cumw[-1] > width
  shrunk <- format(df[, !too_wide, drop = FALSE])
  
  needs_dots <- is.na(rows) || rows > n
  if (needs_dots) {
    dot_width <- pmin(w[-1][!too_wide], 3)
    dots <- vapply(dot_width, function(i) paste(rep(".", i), collapse = ""),
      FUN.VALUE = character(1))
    shrunk <- rbind(shrunk, ".." = dots)
  }
  print(shrunk)

  if (any(too_wide)) {
    vars <- colnames(mat)[too_wide]
    types <- vapply(df[too_wide], type_sum, character(1))
    var_types <- paste0(vars, " (", types, ")", collapse = ", ")
    
    cat(wrap("Variables not shown: ", var_types), "\n", sep = "")
  }
}

wrap <- function(..., indent = 0) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2, 
    width = getOption("width"))
  paste0(wrapped, collapse = "\n")
}

ruler <- function() {
  x <- seq_len(getOption("width"))
  y <- ifelse(x %% 10 == 0, x %/% 10, ifelse(x %% 5 == 0, "+", "-"))
  cat(y, "\n", sep = "")
  cat(x %% 10, "\n", sep = "")
}

#' @S3method print BoolResult
print.BoolResult <- function(x, ...) {
  cat(x)
  if (!x) cat(": ", attr(x, "comment"), sep = "")
  cat("\n")
}
