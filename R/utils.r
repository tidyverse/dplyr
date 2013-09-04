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

compact <- function(x) Filter(Negate(is.null), x)

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



wrap <- function(...) {
  string <- paste0(...)
  wrapped <- strwrap(string, width = getOption("width"), exdent = 2)
  paste0(wrapped, collapse = "\n")
}

deparse_all <- function(x) {
  deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  vapply(x, deparse2, FUN.VALUE = character(1))
}

commas <- function(...) paste0(..., collapse = ", ")
