check_pkg <- function(name, reason) {
  if (requireNamespace(name, quietly = TRUE))
    return(invisible(TRUE))

  glubort('The {name} package is required to {reason}.
    Please install it with `install.packages("{name}")`'
  )
}

# ngettext() does extra work, this function is a simpler version
ntext <- function(n, msg1, msg2) {
  if (n == 1) msg1 else msg2
}

bad <- function(..., .envir = parent.frame()) {
  glubort(NULL, ..., .envir = parent.frame())
}

bad_args <- function(args, ..., .envir = parent.frame()) {
  glubort(hdr_args(args), ..., .envir = .envir)
}

bad_pos_args <- function(pos_args, ..., .envir = parent.frame()) {
  glubort(hdr_pos_args(pos_args), ..., .envir = .envir)
}

bad_calls <- function(calls, ..., .envir = parent.frame()) {
  glubort(hdr_calls(calls), ..., .envir = .envir)
}

bad_named_calls <- function(named_calls, ..., .envir = parent.frame()) {
  glubort(hdr_named_calls(named_calls), ..., .envir = .envir)
}

bad_cols <- function(cols, ..., .envir = parent.frame()) {
  glubort(hdr_cols(cols), ..., .envir = .envir)
}

bad_measures <- function(measures, ..., .envir = parent.frame()) {
  glubort(hdr_measures(measures), ..., .envir = .envir)
}

glubort <- function(header, ..., .envir = parent.frame()) {
  text <- glue(..., .envir = .envir)
  if (!is_null(header)) text <- paste0(header, ": ", text)
  abort(text)
}

hdr_args <- function(...) {
  x <- parse_args(...)
  glue("{fmt_obj(x)}")
}

hdr_pos_args <- function(...) {
  x <- c(...)
  args <- ntext(length(x), "Argument", "Arguments")
  glue("{args} {fmt_comma(x)}")
}

hdr_calls <- function(...) {
  x <- parse_named_call(...)
  glue("{fmt_comma(x)}")
}

hdr_named_calls <- function(...) {
  x <- parse_named_call(...)
  glue("{fmt_named(x)}")
}

hdr_cols <- function(x) {
  cols <- ntext(length(x), "Column", "Columns")
  glue("{cols} {fmt_obj(x)}")
}

hdr_measures <- function(x) {
  measures <- ntext(length(x), "Measure", "Measures")
  glue("{measures} {fmt_obj(x)}")
}

fmt_args <- function(...) {
  x <- parse_args(...)
  fmt_obj(x)
}

parse_args <- function(...) {
  x <- unlist(list(...), recursive = FALSE)
  is_fml <- map_lgl(x, is_formula)
  x[is_fml] <- map_chr(map(x[is_fml], "[[", 2), as_string)
  unlist(x)
}

parse_named_call <- function(...) {
  x <- unlist(list(...), recursive = FALSE)
  x <- map_chr(map(x, f_rhs), deparse_trunc)
  x
}

fmt_named <- function(x) {
  fmt_comma(paste0(fmt_obj1(names2(x)), " = ", x))
}

fmt_obj <- function(x) {
  fmt_comma(fmt_obj1(x))
}

fmt_obj1 <- function(x) {
  if (is.numeric(x)) x
  else paste0("`", x, "`")
}

fmt_classes <- function(x) {
  paste(class(x), collapse = "/")
}

fmt_dims <- function(x) {
  paste0("[", paste0(x, collapse = " x "), "]")
}

fmt_comma <- function(...) {
  MAX_ITEMS <- 6L

  x <- paste0(...)
  if (length(x) > MAX_ITEMS) {
    length(x) <- MAX_ITEMS
    x[[MAX_ITEMS]] <- "..."
  }

  commas(x)
}
