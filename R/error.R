# ngettext() does extra work, this function is a simpler version
ntext <- function(n, msg1, msg2) {
  if (n == 1) msg1 else msg2
}

fmt_pos_args <- function(x) {
  args <- ntext(length(x), "Argument", "Arguments")
  glue("{args} {fmt_comma(x)}")
}

fmt_cols <- function(x) {
  cols <- ntext(length(x), "Column", "Columns")
  glue("{cols} {fmt_obj(x)}")
}

fmt_obj <- function(x) {
  fmt_comma(fmt_obj1(x))
}

fmt_obj1 <- function(x) {
  paste0("`", x, "`")
}

fmt_classes <- function(x) {
  paste(class(x), collapse = "/")
}

fmt_comma <- function(..., .max = 6) {
  x <- paste0(...)
  if (length(x) > .max) {
    length(x) <- .max
    x[[.max]] <- "..."
  }

  commas(x)
}
