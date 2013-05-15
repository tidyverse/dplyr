dots <- function(...) {
  eval(substitute(alist(...)))
}

named_dots <- function(...) {
  args <- dots(...)

  nms <- names2(args)
  missing <- nms == ""
  if (all(!missing)) return(args)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(args[missing], deparse2, character(1), USE.NAMES = FALSE)

  names(args)[missing] <- defaults
  args
}


names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

"%||%" <- function(x, y) if(is.null(x)) y else x

is.wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
  abs(x - round(x)) < tol
}

as_df <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -length(x[[1]]))

  x
}

trunc_rows <- function(x, n = 10) {
  mat <- format(head(x, n))
  dots <- matrix(rep("...", ncol(mat)), nrow = 1,
    dimnames = list("...", colnames(mat)))

  rbind(mat, dots)
}

dim_desc <- function(x) {
  d <- format(dim(x), big.mark = ",", trim = TRUE)
  paste0(" [", paste0(d, collapse = " x "), "]")
}
