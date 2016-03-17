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
  sql_render(sql_build(x$ops, x$src$con, ...), con = x$src$con, ...)
}

#' @export
sql_render.tbl_lazy <- function(x, con = NULL, ...) {
  sql_render(sql_build(x$ops, con = NULL, ...), con = NULL, ...)
}

#' @export
sql_render.select_query <- function(x, con = NULL, ..., root = FALSE) {
  from <- sql_subquery(con, sql_render(x$from, con, ..., root = root), name = NULL)

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

#' @export
sql_render.join_query <- function(query, con = NULL, ..., root = FALSE) {
  from_x <- sql_subquery(con, sql_render(query$x, con, ..., root = root), name = NULL)
  from_y <- sql_subquery(con, sql_render(query$y, con, ..., root = root), name = NULL)

  sql_join(con, from_x, from_y, type = query$type, by = query$by)
}

#' @export
sql_render.semi_join_query <- function(query, con = NULL, ..., root = FALSE) {
  from_x <- sql_subquery(con, sql_render(query$x, con, ..., root = root), name = "_LEFT")
  from_y <- sql_subquery(con, sql_render(query$y, con, ..., root = root), name = "_RIGHT")

  sql_semi_join(con, from_x, from_y, anti = query$anti, by = query$by)
}

#' @export
sql_render.set_op_query <- function(query, con = NULL, ..., root = FALSE) {
  from_x <- sql_render(query$x, con, ..., root = TRUE)
  from_y <- sql_render(query$y, con, ..., root = TRUE)

  sql_set_op(con, from_x, from_y, method = query$type)
}
