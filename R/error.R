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

glubort <- function(..., .envir = parent.frame()) {
  abort(glue(..., .envir = .envir))
}

hdr_args <- function(...) {
  x <- parse_args(...)
  args <- ntext(length(x), "Argument", "Arguments")
  hdr("{args} {fmt_obj(x)}")
}

hdr_pos_args <- function(...) {
  x <- c(...)
  if (length(x) == 1) args <- "Argument"
  else args <- "Arguments"
  hdr("{args} {fmt_comma(x)}")
}

hdr_named_call <- function(...) {
  x <- parse_named_call(...)
  if (length(x) == 1) args <- "Argument"
  else args <- "Arguments"
  hdr("{args} {fmt_named(x)}")
}

hdr_cols <- function(x) {
  if (length(x) == 1) cols <- "Column"
  else cols <- "Columns"
  hdr("{cols} {fmt_obj(x)}")
}

hdr <- function(..., .envir = parent.frame()) {
  x <- glue(..., .envir = .envir)
  gsub("[:]*$", ":", x)
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
  x <- map_chr(map(x, "[[", 2), deparse_trunc)
  x
}

fmt_named <- function(x) {
  fmt_comma(paste0(fmt_obj1(names(x)), " = ", x))
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

fmt_comma <- function(...) {
  MAX_ITEMS <- 6L

  x <- paste0(...)
  if (length(x) > MAX_ITEMS) {
    length(x) <- MAX_ITEMS
    x[[MAX_ITEMS]] <- "..."
  }

  commas(x)
}
