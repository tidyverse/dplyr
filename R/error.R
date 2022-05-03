# ngettext() does extra work, this function is a simpler version
ntext <- function(n, msg1, msg2) {
  if (n == 1) msg1 else msg2
}

fmt_pos_args <- function(x) {
  args <- ntext(length(x), "Argument", "Arguments")
  glue("{args} {fmt_comma(x)}")
}

fmt_calls <- function(...) {
  x <- parse_named_call(...)
  fmt_obj(x)
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
