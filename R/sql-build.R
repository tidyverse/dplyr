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
sql_build <- function(op, ...) {
  UseMethod("sql_build")
}

#' @export
sql_build.tbl_lazy <- function(op, ...) {
  sql_build(op$ops, ...)
}

#' @export
sql_build.op_base_remote <- function(op, ...) {
  op$x
}

#' @export
sql_build.op_base_local <- function(op, ...) {
  ident("df")
}

#' @export
sql_build.op_select <- function(op, ...) {
  vars <- select_vars_(op_vars(op$x), op$dots, include = op_grps(op$x))
  select_query(sql_build(op$x), ident(vars))
}

#' @export
sql_build.op_rename <- function(op, ...) {
  vars <- rename_vars_(op_vars(op$x), op$dots)
  select_query(sql_build(op$x), vars)
}

#' @export
sql_build.op_arrange <- function(op, ...) {
  order_vars <- translate_sql_(op$dots, vars = op_vars(op))
  group_vars <- ident(op_grps(op$x))

  select_query(sql_build(op$x), order_by = c(group_vars, order_vars))
}

#' @export
sql_build.op_summarise <- function(op, ...) {
  select_vars <- translate_sql_(op$dots, vars = op_vars(op), window = FALSE)
  group_vars <- ident(op_grps(op$x))

  select_query(
    sql_build(op$x, con),
    select = c(group_vars, select_vars),
    group_by = group_vars
  )
}

#' @export
sql_build.op_group_by <- function(op, ...) {
  sql_build(op$x)
}

#' @export
sql_build.op_filter <- function(op, ...) {
  # TODO: multistage filter if computations involved
  where_sql <- translate_sql_(op$dots, vars = op_vars(op))

  select_query(
    sql_build(op$x, con),
    where = where_sql
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
                         limit = NULL,
                         offset = NULL) {

  stopifnot(is.character(select))
  stopifnot(is.character(where))
  stopifnot(is.character(group_by))
  stopifnot(is.character(having))
  stopifnot(is.character(order_by))
  stopifnot(is.null(limit) || (is.integer(limit) && length(limit) == 1L))
  stopifnot(is.null(offset) || (is.integer(offset) && length(offset) == 1L))

  structure(
    list(
      from = from,
      select = select,
      where = where,
      group_by = group_by,
      having = having,
      order_by = order_by,
      limit = limit,
      offset = offset
    ),
    class = "select_query"
  )
}

#' @export
print.select_query <- function(x, ...) {
  cat("<SQL SELECT>\n")
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
sql_render.tbl_lazy <- function(x, con = NULL, ...) {
  sql_render(sql_build(x$ops, ...), con = con, ...)
}

#' @export
sql_render.select_query <- function(x, con = NULL, ...) {
  from <- sql_subquery(con, sql_render(x$from, con, ..., root = FALSE))

  sql_select(
    con, x$select, from, where = x$where, group_by = x$group_by,
    having = x$having, order_by = x$order_by, limit = x$limit,
    offset = x$offset, ...
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
