#' Lazy operations
#'
#' This set of S3 classes describe the action of dplyr verbs. These are
#' currently used for SQL sources to separate the description of operations
#' in R from their computation in SQL. This API is very new so is likely
#' to evolve in the future.
#'
#' `op_vars()` and `op_grps()` compute the variables and groups from
#' a sequence of lazy operations. `op_sort()` tracks the order of the
#' data for use in window functions.
#'
#' @keywords internal
#' @name lazy_ops
NULL

# Base constructors -------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_base <- function(x, vars, class = character()) {
  stopifnot(is.character(vars))

  structure(
    list(
      x = x,
      vars = vars
    ),
    class = c(paste0("op_base_", class), "op_base", "op")
  )

}

op_base_remote <- function(x, vars) {
  stopifnot(is.sql(x))
  op_base(x, vars, class = "remote")
}

#' @export
print.op_base_remote <- function(x, ...) {
  if (inherits(x$x, "ident")) {
    cat("From: ", x$x, "\n", sep = "")
  } else {
    cat("From: <derived table>\n")
  }

  cat("<Table: ", x$x, ">\n", sep = "")
}

op_base_local <- function(df) {
  op_base(df, names(df), class = "local")
}

#' @export
print.op_base_local <- function(x, ...) {
  cat("<Local data frame> ", dim_desc(x$x), "\n", sep = "")
}

# Operators ---------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_single <- function(name, x, dots = list(), args = list()) {
  structure(
    list(
      name = name,
      x = x,
      dots = dots,
      args = args
    ),
    class = c(paste0("op_", name), "op_single", "op")
  )
}

#' @export
#' @rdname lazy_ops
add_op_single <- function(name, .data, dots = list(), args = list()) {
  .data$ops <- op_single(name, x = .data$ops, dots = dots, args = args)
  .data
}

#' @export
print.op_single <- function(x, ...) {
  print(x$x)

  cat("-> ", x$name, "()\n", sep = "")
  for (dot in x$dots) {
    cat("   - ", deparse_trunc(dot), "\n", sep = "")
  }
}

#' @export
#' @rdname lazy_ops
op_double <- function(name, x, y, args = list()) {
  structure(
    list(
      name = name,
      x = x,
      y = y,
      args = args
    ),
    class = c(paste0("op_", name), "op_double", "op")
  )
}

# op_grps -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_grps <- function(op) UseMethod("op_grps")
#' @export
op_grps.op_base <- function(op) character()
#' @export
op_grps.op_group_by <- function(op) {
  if (isTRUE(op$args$add)) {
    union(op_grps(op$x), names(op$dots))
  } else {
    names(op$dots)
  }
}
#' @export
op_grps.op_ungroup <- function(op) {
  NULL
}
#' @export
op_grps.op_summarise <- function(op) {
  grps <- op_grps(op$x)
  if (length(grps) == 1) {
    NULL
  } else {
    grps[-length(grps)]
  }
}

#' @export
op_grps.op_rename <- function(op) {
  names(rename_vars_(op_grps(op$x), op$dots))
}

#' @export
op_grps.op_single <- function(op) {
  op_grps(op$x)
}
#' @export
op_grps.op_double <- function(op) {
  op_grps(op$x)
}

#' @export
op_grps.tbl_lazy <- function(op) {
  op_grps(op$ops)
}

# op_vars -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_vars <- function(op) UseMethod("op_vars")

#' @export
op_vars.op_base <- function(op) {
  op$vars
}
#' @export
op_vars.op_select <- function(op) {
  names(op$dots)
}
#' @export
op_vars.op_rename <- function(op) {
  names(rename_vars_(op_vars(op$x), op$dots))
}
#' @export
op_vars.op_summarise <- function(op) {
  c(op_grps(op$x), names(op$dots))
}
#' @export
op_vars.op_distinct <- function(op) {
  if (length(op$dots) == 0 || op$args$.keep_all) {
    op_vars(op$x)
  } else  {
    c(op_grps(op$x), names(op$dots))
  }
}
#' @export
op_vars.op_mutate <- function(op) {
  unique(c(op_vars(op$x), names(op$dots)))
}
#' @export
op_vars.op_single <- function(op) {
  op_vars(op$x)
}
#' @export
op_vars.op_join <- function(op) {
  unlist(op$args$vars, use.names = FALSE)
}
#' @export
op_vars.op_semi_join <- function(op) {
  op_vars(op$x)
}
#' @export
op_vars.op_set_op <- function(op) {
  op_vars(op$x)
}
#' @export
op_vars.tbl_lazy <- function(op) {
  op_vars(op$ops)
}

# op_sort -----------------------------------------------------------------

# This is only used to determine the order for window functions
# so it purposely ignores grouping. Returns a list of expressions.

#' @export
#' @rdname lazy_ops
op_sort <- function(op) UseMethod("op_sort")
#' @export
op_sort.op_base <- function(op) NULL

#' @export
op_sort.op_summarise <- function(op) NULL

#' @export
op_sort.op_arrange <- function(op) {
  c(op_sort(op$x), op$dots)
}

#' @export
op_sort.op_single <- function(op) {
  op_sort(op$x)
}

#' @export
op_sort.op_double <- function(op) {
  op_sort(op$x)
}

#' @export
op_sort.tbl_lazy <- function(op) {
  op_sort(op$ops)
}

# We want to preserve this ordering (for window functions) without
# imposing an additional arrange, so we have a special op_order

add_op_order <- function(.data, dots = list()) {
  if (length(dots) == 0) {
    return(.data)
  }

  .data$ops <- op_single("order", x = .data$ops, dots = dots)
  .data
}
#' @export
op_sort.op_order <- function(op) {
  c(op_sort(op$x), op$dots)
}

#' @export
sql_build.op_order <- function(op, con, ...) {
  sql_build(op$x, con, ...)
}


# Description -------------------------------------------------------------

tbl_desc <- function(x) {
  paste0(
    op_desc(x$ops),
    " [",
    op_rows(x$ops),
    " x ",
    big_mark(op_cols(x$ops)),
    "]"
  )
}

op_rows <- function(op) "??"
op_cols <- function(op) length(op_vars(op))

op_desc <- function(op) UseMethod("op_desc")

op_desc.op_base_remote <- function(op) {
  if (is.ident(op$x)) {
    paste0("table<", op$x, ">")
  } else {
    "SQL"
  }
}

#' @export
op_desc.op_group_by <- function(x, ...) {
  op_desc(x$x, ...)
}

#' @export
op_desc.op_arrange <- function(x, ...) {
  op_desc(x$x, ...)
}

#' @export
op_desc.op <- function(x, ..., con = con) {
  "lazy query"
}
