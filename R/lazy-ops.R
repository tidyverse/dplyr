#' Lazy operations
#'
#' This set of S3 classes describe the action of dplyr verbs. These are
#' currently used for SQL sources to separate the description of operations
#' in R from their computation in SQL. This API is very new so is likely
#' to evolve in the future.
#'
#' \code{op_vars} and \code{op_grps} compute the variables and groups from
#' a sequence of lazy operations. \code{op_sort} tracks the order of the
#' data for use in window functions.
#'
#' @keywords internal
#' @name lazy_ops
NULL

op_base_remote <- function(src, x, vars = NULL) {
  # If not literal sql, must be a table identifier
  if (!is.sql(x)) {
    x <- ident(x)
  }

  if (is.null(vars)) {
    vars <- db_query_fields(src$con, x)
  }
  op_base("remote", src, x, vars)
}

#' @export
print.op_base_remote <- function(x, ...) {
  cat("Source: ", src_desc(x$src), "\n", sep = "")

  if (inherits(x$x, "ident")) {
    cat("From: ", x$x, "\n", sep = "")
  } else {
    cat("From: <derived table>\n")
  }

  cat("<Table: ", x$x, ">\n", sep = "")
}

op_base_local <- function(df, env = parent.frame()) {
  op_base("local", src_df(env = env), df, names(df))
}

#' @export
print.op_base_local <- function(x, ...) {
  cat("<Local data frame> ", dim_desc(x$x), "\n", sep = "")
}

#' @export
#' @rdname lazy_ops
op_base <- function(name, src, x, vars) {
  stopifnot(is.character(vars))

  structure(
    list(
      src = src,
      x = x,
      vars = vars
    ),
    class = c(paste0("op_base_", name), "op_base", "op")
  )

}

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
print.op_single <- function(x, ...) {
  print(x$x)

  cat("-> ", x$name, "()\n", sep = "")
  for (dot in x$dots) {
    cat("   - ", deparse_trunc(dot$expr), "\n", sep = "")
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

# op_vars -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_vars <- function(x) UseMethod("op_vars")

#' @export
op_vars.op_base <- function(x) {
  x$vars
}
#' @export
op_vars.op_select <- function(x) {
  names(select_vars_(op_vars(x$x), x$dots, include = op_grps(x$x)))
}
#' @export
op_vars.op_rename <- function(x) {
  names(rename_vars_(op_vars(x$x), x$dots))
}
#' @export
op_vars.op_summarise <- function(x) {
  c(op_grps(x$x), names(x$dots))
}
#' @export
op_vars.op_mutate <- function(x) {
  c(op_vars(x$x), names(x$dots))
}
#' @export
op_vars.op_single <- function(x) {
  op_vars(x$x)
}
#' @export
op_vars.op_join <- function(x) {
  by <- x$args$by
  x_vars <- op_vars(x$x)
  y_vars <- op_vars(x$y)

  unique <- unique_names(x_vars, y_vars, by = by, suffix = x$args$suffix)

  if (is.null(unique)) {
    c(by$x, setdiff(x_vars, by$x), setdiff(y_vars, by$y))
  } else {
    union(unique$x, unique$y)
  }
}
#' @export
op_vars.op_semi_join <- function(x) {
  op_vars(x$x)
}
#' @export
op_vars.op_set_op <- function(x) {
  op_vars(x$x)
}
#' @export
op_vars.tbl_lazy <- function(x) {
  op_vars(x$ops)
}

# op_grps -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_grps <- function(x) UseMethod("op_grps")
#' @export
op_grps.op_base <- function(x) character()
#' @export
op_grps.op_group_by <- function(x) {
  if (isTRUE(x$args$add)) {
    union(op_grps(x$x), names(x$dots))
  } else {
    names(x$dots)
  }
}
#' @export
op_grps.op_ungroup <- function(x) {
  character()
}
#' @export
op_grps.op_summarise <- function(x) {
  grps <- op_grps(x$x)
  if (length(grps) == 1) {
    character()
  } else {
    grps[-length(grps)]
  }
}
#' @export
op_grps.op_single <- function(x) {
  op_grps(x$x)
}
#' @export
op_grps.op_double <- function(x) {
  op_grps(x$x)
}

#' @export
op_grps.tbl_lazy <- function(x) {
  op_grps(x$ops)
}


# op_vars -----------------------------------------------------------------

#' @export
#' @rdname lazy_ops
op_vars <- function(x) UseMethod("op_vars")

#' @export
op_vars.op_base <- function(x) {
  x$vars
}
#' @export
op_vars.op_select <- function(x) {
  names(select_vars_(op_vars(x$x), x$dots, include = op_grps(x$x)))
}
#' @export
op_vars.op_rename <- function(x) {
  names(rename_vars_(op_vars(x$x), x$dots))
}
#' @export
op_vars.op_summarise <- function(x) {
  c(op_grps(x$x), names(x$dots))
}
#' @export
op_vars.op_mutate <- function(x) {
  c(op_vars(x$x), names(x$dots))
}
#' @export
op_vars.op_single <- function(x) {
  op_vars(x$x)
}
#' @export
op_vars.op_join <- function(x) {
  by <- x$args$by
  x_vars <- op_vars(x$x)
  y_vars <- op_vars(x$y)

  unique <- unique_names(x_vars, y_vars, by = by, suffix = x$args$suffix)

  if (is.null(unique)) {
    c(by$x, setdiff(x_vars, by$x), setdiff(y_vars, by$y))
  } else {
    union(unique$x, unique$y)
  }
}
#' @export
op_vars.op_semi_join <- function(x) {
  op_vars(x$x)
}
#' @export
op_vars.op_set_op <- function(x) {
  op_vars(x$x)
}
#' @export
op_vars.tbl_lazy <- function(x) {
  op_vars(x$ops)
}

# op_sort -----------------------------------------------------------------

# This is only used to determine the order for window functions
# so it purposely ignores grouping.

#' @export
#' @rdname lazy_ops
op_sort <- function(x) UseMethod("op_sort")
#' @export
op_sort.op_base <- function(x) NULL

#' @export
op_sort.op_summarise <- function(x) NULL

#' @export
op_sort.op_arrange <- function(x) {
  order_vars <- translate_sql_(x$dots, NULL, op_vars(x))
  c.sql(op_sort(x$x), order_vars, drop_null = TRUE)
}

#' @export
op_sort.op_single <- function(x) {
  op_sort(x$x)
}

#' @export
op_sort.op_double <- function(x) {
  op_sort(x$x)
}

#' @export
op_sort.tbl_lazy <- function(x) {
  op_sort(x$ops)
}
