#' Build and render SQL from a sequence of lazy operations
#'
#' \code{sql_build} creates a \code{select_query} S3 object, that is rendered
#' to a SQL string by \code{sql_render}. \code{sql_build} is particularly
#' well suited for testing.
#'
#' \code{sql_build} is generic over the lazy operations, \link{lazy_ops}.
#' If a database does not follow regular ANSI 92 rules for creating a select
#' query, you can provide a methods for \code{sql_select} and
#' \code{sql_subquery}.
#'
#' @export
#' @keywords internal
#' @param op A sequence of lazy operations
#' @param con A database connection. The default \code{NULL} uses a set of
#'   rules that should be very similar to ANSI 92, and allows for testing
#'   without an active database connection.
#' @param ... Other arguments passed on to the methods. Not currently used.
sql_build <- function(op, con, ...) {
  UseMethod("sql_build")
}

#' @export
sql_build.tbl_sql <- function(op, con, ...) {
  sql_build(op$ops, op$con, ...)
}

#' @export
sql_build.tbl_lazy <- function(op, con = NULL, ...) {
  sql_build(op$ops, con, ...)
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
  vars <- select_vars_(op_vars(op$x), op$dots, include = op_grps(op$x))
  select_query(sql_build(op$x, con), ident(vars))
}

#' @export
sql_build.op_rename <- function(op, con, ...) {
  vars <- rename_vars_(op_vars(op$x), op$dots)
  select_query(sql_build(op$x, con), vars)
}

#' @export
sql_build.op_arrange <- function(op, con, ...) {
  order_vars <- translate_sql_(op$dots, con, op_vars(op))
  group_vars <- ident(op_grps(op$x))

  select_query(sql_build(op$x, con), order_by = c(group_vars, order_vars))
}

#' @export
sql_build.op_summarise <- function(op, con, ...) {
  select_vars <- translate_sql_(op$dots, con, op_vars(op), window = FALSE)
  group_vars <- ident(op_grps(op$x))

  select_query(
    sql_build(op$x, con),
    select = c(group_vars, select_vars),
    group_by = group_vars
  )
}

#' @export
sql_build.op_mutate <- function(op, con, ...) {
  vars <- op_vars(op$x)

  new_vars <- translate_sql_(op$dots, con, vars)
  old_vars <- ident(vars)

  select_query(
    sql_build(op$x, con),
    select = c(old_vars, new_vars)
  )
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
  # TODO: multistage filter if computations involved
  where_sql <- translate_sql_(op$dots, vars = op_vars(op))

  select_query(
    sql_build(op$x, con),
    where = where_sql
  )
}


sql_build.op_distinct <- function(op, con, ...) {
  if (length(op$dots) > 0 && !op$args$.keep_all) {
    stop("Can't calculate distinct only on specified columns with SQL",
      call. = FALSE)
  }

  select_query(
    sql_build(op$x, con),
    distinct = TRUE
  )
}

# Dual table ops --------------------------------------------------------

sql_build.op_join <- function(op, con, ...) {
  # Ensure tables have unique names
  x_names <- op_vars(op$x)
  y_names <- op_vars(op$y)
  by <- op$args$by

  uniques <- unique_names(
    x_names, y_names, by = by$x[by$x == by$y], suffix = op$args$suffix
  )

  if (is.null(uniques)) {
    x <- op$x
    y <- op$y
  } else {
    # TODO: it would be better to construct an explicit FROM statement
    # that used the table names to disambiguate the fields names: this
    # would remove a layer of subqueries and would make sql_join more
    # flexible.
    x <- select_(op$x, setNames(x_names, uniques$x))
    y <- select_(op$y, setNames(y_names, uniques$y))

    by$x <- unname(uniques$x[by$x])
    by$y <- unname(uniques$y[by$y])
  }

  join_query(x, y,
    type = op$args$type,
    by = by
  )
}
