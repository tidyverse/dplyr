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

#' @export
sql_build.op_base_remote <- function(op, con, ...) {
  op$x
}

#' @export
sql_build.op_base_local <- function(op, con, ...) {
  ident("df")
}

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

# select_query ------------------------------------------------------------

#' @export
#' @rdname sql_build
select_query <- function(from,
                         select = sql("*"),
                         where = character(),
                         group_by = character(),
                         having = character(),
                         order_by = character(),
                         distinct = FALSE) {

  stopifnot(is.character(select))
  stopifnot(is.character(where))
  stopifnot(is.character(group_by))
  stopifnot(is.character(having))
  stopifnot(is.character(order_by))
  stopifnot(is.logical(distinct), length(distinct) == 1L)

  structure(
    list(
      from = from,
      select = select,
      where = where,
      group_by = group_by,
      having = having,
      order_by = order_by,
      distinct = distinct
    ),
    class = "select_query"
  )
}

#' @export
print.select_query <- function(x, ...) {
  cat("<SQL SELECT", if (x$distinct) " DISTINCT", ">\n", sep = "")
  cat("From:     ", x$from, "\n", sep = "")

  if (length(x$select))   cat("Select:   ", named_commas(x$select), "\n", sep = "")
  if (length(x$where))    cat("Where:    ", named_commas(x$where), "\n", sep = "")
  if (length(x$group_by)) cat("Group by: ", named_commas(x$group_by), "\n", sep = "")
  if (length(x$order_by)) cat("Order by: ", named_commas(x$order_by), "\n", sep = "")
  if (length(x$having))   cat("Having:   ", named_commas(x$having), "\n", sep = "")
}

# sql_render --------------------------------------------------------------

#' @export
#' @rdname sql_build
sql_render <- function(x, con = NULL, ...) {
  UseMethod("sql_render")
}

#' @export
sql_render.op <- function(x, con = NULL, ...) {
  sql_render(sql_build(x, ...), con = con, ...)
}

#' @export
sql_render.tbl_sql <- function(x, con = NULL, ...) {
  sql_render(sql_build(x$ops, con, ...), con = x$src$con, ...)
}

#' @export
sql_render.tbl_lazy <- function(x, con = NULL, ...) {
  sql_render(sql_build(x$ops, ...), con = NULL, ...)
}

#' @export
sql_render.select_query <- function(x, con = NULL, ..., root = FALSE) {
  from <- sql_subquery(con, sql_render(x$from, con, ..., root = root))

  sql_select(
    con, x$select, from, where = x$where, group_by = x$group_by,
    having = x$having, order_by = x$order_by, distinct = x$distinct, ...
  )
}

#' @export
sql_render.ident <- function(x, con = NULL, ..., root = TRUE) {
  if (root) {
    sql_select(con, sql("*"), x)
  } else {
    x
  }
}

#' @export
sql_render.sql <- function(x, con = NULL, ...) {
  x
}
