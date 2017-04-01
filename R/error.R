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

glubort <- function(..., args = NULL, pos_args = NULL,
                    calls = NULL, named_calls = NULL,
                    cols = NULL, measures = NULL,
                    .envir = parent.frame()) {
  text <- glue(..., .envir = .envir)
  hdr <- NULL
  if (!is_null(args)) {
    hdr <- hdr_args(args)
  } else if (!is_null(pos_args)) {
    hdr <- hdr_pos_args(pos_args)
  } else if (!is_null(calls)) {
    hdr <- hdr_call(calls)
  } else if (!is_null(named_calls)) {
    hdr <- hdr_named_call(named_calls)
  } else if (!is_null(cols)) {
    hdr <- hdr_cols(cols)
  } else if (!is_null(measures)) {
    hdr <- hdr_measures(measures)
  }

  if (!is_null(hdr)) text <- paste0(hdr, " ", text)
  abort(text)
}

hdr_args <- function(...) {
  x <- parse_args(...)
  hdr("{fmt_obj(x)}")
}

hdr_pos_args <- function(...) {
  x <- c(...)
  args <- ntext(length(x), "Argument", "Arguments")
  hdr("{args} {fmt_comma(x)}")
}

hdr_call <- function(...) {
  x <- parse_named_call(...)
  hdr("{fmt_comma(x)}")
}

hdr_named_call <- function(...) {
  x <- parse_named_call(...)
  hdr("{fmt_named(x)}")
}

hdr_cols <- function(x) {
  cols <- ntext(length(x), "Column", "Columns")
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

fmt_comma <- function(...) {
  MAX_ITEMS <- 6L

  x <- paste0(...)
  if (length(x) > MAX_ITEMS) {
    length(x) <- MAX_ITEMS
    x[[MAX_ITEMS]] <- "..."
  }

  commas(x)
}
