check_pkg <- function(name, reason, install = TRUE) {
  if (is_installed(name)) {
    return(invisible(TRUE))
  }

  glubort(NULL, "The {name} package is required to {reason}.",
    if (install) '\nPlease install it with `install.packages("{name}")`' else ""
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
  glubort(fmt_args(args), ..., .envir = .envir)
}

bad_pos_args <- function(pos_args, ..., .envir = parent.frame()) {
  glubort(fmt_pos_args(pos_args), ..., .envir = .envir)
}

bad_calls <- function(calls, ..., .envir = parent.frame()) {
  glubort(fmt_calls(calls), ..., .envir = .envir)
}

bad_named_calls <- function(named_calls, ..., .envir = parent.frame()) {
  glubort(fmt_named_calls(named_calls), ..., .envir = .envir)
}

bad_eq_ops <- function(named_calls, ..., .envir = parent.frame()) {
  glubort(NULL, ..., "\n", fmt_wrong_eq_ops(named_calls), sep="", .envir = .envir)
}

bad_cols <- function(cols, ..., .envir = parent.frame()) {
  glubort(fmt_cols(cols), ..., .envir = .envir)
}

bad_measures <- function(measures, ..., .envir = parent.frame()) {
  glubort(fmt_measures(measures), ..., .envir = .envir)
}

glubort <- function(header, ..., .envir = parent.frame(), .abort = abort) {
  text <- glue(..., .envir = .envir)
  if (!is_null(header)) text <- paste0(header, " ", text)
  .abort(text)
}

fmt_args <- function(x) {
  x <- parse_args(x)
  fmt_obj(x)
}

fmt_pos_args <- function(x) {
  args <- ntext(length(x), "Argument", "Arguments")
  glue("{args} {fmt_comma(x)}")
}

fmt_calls <- function(...) {
  x <- parse_named_call(...)
  fmt_obj(x)
}

fmt_named_calls <- function(...) {
  x <- parse_named_call(...)
  fmt_named(x)
}

fmt_wrong_eq_ops <- function(...) {
  x <- parse_named_call(...)
  fmt_items(
    paste0("Did you mean ", fmt_obj1(paste0(names2(x), " == ", x)), "?"),
    bullet = "*"
  )
}

fmt_cols <- function(x) {
  cols <- ntext(length(x), "Column", "Columns")
  glue("{cols} {fmt_obj(x)}")
}

fmt_measures <- function(x) {
  measures <- ntext(length(x), "Measure", "Measures")
  glue("{measures} {fmt_obj(x)}")
}

fmt_named <- function(x) {
  fmt_comma(paste0(fmt_obj1(names2(x)), " = ", x))
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

fmt_dims <- function(x) {
  paste0("[", paste0(x, collapse = " x "), "]")
}

fmt_comma <- function(..., .max = 6) {
  x <- paste0(...)
  if (length(x) > .max) {
    length(x) <- .max
    x[[.max]] <- "..."
  }

  commas(x)
}

fmt_items <- function(x, bullet = "-", .max = 6) {
  if (length(x) > .max) {
    more <- glue("({length(x) - (.max - 1)} more)")
    length(x) <- .max
    x[.max] <- more
  }

  paste0(glue("{bullet} {x}"), collapse = "\n")
}

parse_args <- function(x) {
  # convert single formula to list of length 1
  x <- unlist(list(x), recursive = FALSE)
  is_fml <- map_lgl(x, is_formula)
  x[is_fml] <- map_chr(map(x[is_fml], "[[", 2), as_string)
  unlist(x)
}

parse_named_call <- function(x) {
  map_chr(x, quo_text)
}


# From rlang
friendly_type_of <- function(x) {
  if (is.object(x)) {
    sprintf("a `%s` object", fmt_classes(x))
  } else {
    as_friendly_type(typeof(x))
  }
}
as_friendly_type <- function(type) {
  switch(type,
    logical = "a logical vector",
    integer = "an integer vector",
    numeric = ,
    double = "a double vector",
    complex = "a complex vector",
    character = "a character vector",
    raw = "a raw vector",
    string = "a string",
    list = "a list",

    NULL = "NULL",
    environment = "an environment",
    externalptr = "a pointer",
    weakref = "a weak reference",
    S4 = "an S4 object",

    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    expression = "an expression vector",
    quosure = "a quosure",
    formula = "a formula",

    char = "an internal string",
    promise = "an internal promise",
    ... = "an internal dots object",
    any = "an internal `any` object",
    bytecode = "an internal bytecode object",

    primitive = ,
    builtin = ,
    special = "a primitive function",
    closure = "a function",

    type
  )
}
