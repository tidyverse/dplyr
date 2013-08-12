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
  d <- format(dim(x), big.mark = ",", trim = TRUE)
  paste0("[", paste0(d, collapse = " x "), "]")
}

#' @export
#' @rdname dplyr-formatting
trunc_mat <- function(x, n = 10L) {
  df <- as.data.frame(head(x, n))
  mat <- format(df)

  width <- getOption("width")

  values <- c(format(rownames(mat))[[1]], unlist(mat[1, ]))
  names <- c("", colnames(mat))
  w <- pmax(nchar(values), nchar(names))
  cumw <- cumsum(w + 1)

  too_wide <- cumw[-1] > width
  shrunk <- mat[, !too_wide, drop = FALSE]

  needs_dots <- nrow(x) > n
  if (needs_dots) {
    dot_width <- pmin(w[-1][!too_wide], 3)
    dots <- vapply(dot_width, function(i) paste(rep(".", i), collapse = ""),
      FUN.VALUE = character(1))
    shrunk <- rbind(shrunk, "." = dots)
  }
  print(shrunk)

  if (any(too_wide)) {
    vars <- colnames(mat)[too_wide]
    types <- vapply(df[too_wide], type_sum, character(1))
    var_types <- paste0(vars, " (", types, ")", collapse = ", ")
    
    cat(wrap("Variables not shown: ", var_types), "\n", sep = "")
  }
}
