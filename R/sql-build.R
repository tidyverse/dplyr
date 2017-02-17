#' Build and render SQL from a sequence of lazy operations
#'
#' `sql_build()` creates a `select_query` S3 object, that is rendered
#' to a SQL string by `sql_render()`. The output from `sql_build()` is
#' designed to be easy to test, as it's database agnostic, and has
#' a hierarchical structure.
#'
#' `sql_build()` is generic over the lazy operations, \link{lazy_ops},
#' and generates an S3 object that represents the query. `sql_render()`
#' takes a query object and then calls a function that is generic
#' over the database. For example, `sql_build.op_mutate()` generates
#' a `select_query`, and `sql_render.select_query()` calls
#' `sql_select()`, which has different methods for different databases.
#' The default methods should generate ANSI 92 SQL where possible, so you
#' backends only need to override the methods if the backend is not ANSI
#' compliant.
#'
#' @export
#' @keywords internal
#' @param op A sequence of lazy operations
#' @param con A database connection. The default `NULL` uses a set of
#'   rules that should be very similar to ANSI 92, and allows for testing
#'   without an active database connection.
#' @param ... Other arguments passed on to the methods. Not currently used.
sql_build <- function(op, con = NULL, ...) {
  UseMethod("sql_build")
}

#' @export
sql_build.tbl_lazy <- function(op, con = NULL, ...) {
  qry <- sql_build(op$ops, con, ...)
  sql_optimise(qry, con = con, ...)
}

# Base ops --------------------------------------------------------

#' @export
sql_build.op_base_remote <- function(op, con, ...) {
  op$x
}

#' @export
sql_build.op_base_local <- function(op, con, ...) {
  ident("df")
}

# Single table ops --------------------------------------------------------

#' @export
sql_build.op_select <- function(op, con, ...) {
  vars <- select_vars(op_vars(op$x), !!! op$dots, include = op_grps(op$x))
  select_query(sql_build(op$x, con), ident(vars))
}

#' @export
sql_build.op_rename <- function(op, con, ...) {
  vars <- rename_vars_(op_vars(op$x), op$dots)
  select_query(sql_build(op$x, con), ident(vars))
}

#' @export
sql_build.op_arrange <- function(op, con, ...) {
  order_vars <- translate_sql_(op$dots, con)
  group_vars <- c.sql(ident(op_grps(op$x)), con = con)

  select_query(sql_build(op$x, con), order_by = order_vars)
}

#' @export
sql_build.op_summarise <- function(op, con, ...) {
  select_vars <- translate_sql_(op$dots, con, window = FALSE)
  group_vars <- c.sql(ident(op_grps(op$x)), con = con)

  select_query(
    sql_build(op$x, con),
    select = c.sql(group_vars, select_vars, con = con),
    group_by = group_vars
  )
}

#' @export
sql_build.op_mutate <- function(op, con, ...) {
  vars <- op_vars(op$x)

  new_vars <- translate_sql_(
    op$dots, con,
    vars_group = op_grps(op),
    vars_order = translate_sql_(op_sort(op), con)
  )
  old_vars <- ident(setdiff(vars, names(new_vars)))

  select_query(
    sql_build(op$x, con),
    select = c.sql(old_vars, new_vars, con = con)
  )
}

#' @export
sql_build.op_head <- function(op, con, ...) {
  select_query(sql_build(op$x, con), limit = op$args$n)
}

#' @export
sql_build.op_group_by <- function(op, con, ...) {
  sql_build(op$x, con, ...)
}

#' @export
sql_build.op_ungroup <- function(op, con, ...) {
  sql_build(op$x, con, ...)
}

#' @export
sql_build.op_filter <- function(op, con, ...) {
  vars <- op_vars(op$x)

  if (!uses_window_fun(op$dots, con)) {
    where_sql <- translate_sql_(op$dots, con)

    select_query(
      sql_build(op$x, con),
      where = where_sql
    )
  } else {
    # Do partial evaluation, then extract out window functions
    where <- translate_window_where_all(op$dots, ls(sql_translate_env(con)$window))

    # Convert where$expr back to a lazy dots object, and then
    # create mutate operation
    mutated <- sql_build(op_single("mutate", op$x, dots = where$comp), con)
    where_sql <- translate_sql_(where$expr, con = con)

    select_query(mutated, select = ident(vars), where = where_sql)
  }
}

#' @export
sql_build.op_distinct <- function(op, con, ...) {
  if (length(op$dots) == 0) {
    select_query(
      sql_build(op$x, con),
      distinct = TRUE
    )
  } else {
    if (op$args$.keep_all) {
      stop(
        "Can't calculate distinct only on specified columns with SQL unless .keep_all is FALSE",
        call. = FALSE
      )
    }

    group_vars <- c.sql(ident(op_vars(op)), con = con)
    select_query(
      sql_build(op$x, con),
      select = group_vars,
      group_by = group_vars
    )
  }
}

# Dual table ops --------------------------------------------------------

#' @export
sql_build.op_join <- function(op, con, ...) {
  join_query(
    op$x, op$y,
    vars = op$args$vars,
    type = op$args$type,
    by = op$args$by,
    suffix = op$args$suffix
  )
}

#' @export
sql_build.op_semi_join <- function(op, con, ...) {
  semi_join_query(op$x, op$y, anti = op$args$anti, by = op$args$by)
}

#' @export
sql_build.op_set_op <- function(op, con, ...) {
  set_op_query(op$x, op$y, type = op$args$type)
}
