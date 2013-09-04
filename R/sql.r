# Build an sql select query from a sql tbl.
sql_select <- function(x, select = NULL, where = NULL, order_by = NULL, ...,
                       n = -1L, explain = getOption("dplyr.explain_sql"),
                       show = getOption("dplyr.show_sql"),
                       into_table = NULL) {
  assert_that(is.tbl(x))
  assert_that(is.numeric(n), length(n) == 1)

  select <- select %||% x$select %||% sql("*")
  where <- where %||% trans_sqlite(x$filter)
  order_by <- order_by %||% trans_sqlite(x$arrange)

  sql <- select_query(
    from = x$table,
    select = select,
    where = where,
    order_by = order_by,
    ...)

  if (is.null(into_table)) {
    exec_sql(x$src$con, sql, n = n, explain = explain, show = show)  
  } else {
    sql <- build_sql("CREATE TEMPORARY TABLE ", ident(into_table), " AS ", sql)
    exec_sql(x$src$con, sql, n = n, explain = explain, show = show, 
      fetch = FALSE)  
  } 
}

#' Generate an SQL select query.
#'
#' Goal is to make valid sql select query inputs - this knows nothing about sources.
#'
#' @keywords internal
#' @param select a character vector of fields to select. Names are used to
#'   create \code{AS} aliases.
#' @param from a string giving the table name
#' @param from,where,group_by,having,order_by,limit,offset Select query 
#'   components. All inputs are \code{\link{escape}}d, so make sure they have
#'   been wrapped appropriately with \code{\link{sql}} or \code{\link{ident}}.
#' @export
#' @examples
#' select_query(sql("*"), ident("mytable"))
#' select_query(sql("*"), ident("mytable"), sql("1 = 0"))
select_query <- function(select, from, where = NULL, group_by = NULL,
                         having = NULL, order_by = NULL, limit = NULL,
                         offset = NULL) {

  out <- vector("list", 8)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
    "limit", "offset")

  assert_that(is.character(select), length(select) > 0L)
  out$select <- build_sql("SELECT ", select)

  assert_that(is.character(from), length(from) == 1L)
  out$from <- build_sql("FROM ", from)

  if (length(where) > 0L) {
    assert_that(is.character(where))
    out$where <- build_sql("WHERE ", escape(where, collapse = " AND "))
  }

  if (!is.null(group_by)) {
    assert_that(is.character(group_by), length(group_by) > 0L)
    out$group_by <- build_sql("GROUP BY ", escape(group_by, collapse = ", "))
  }

  if (!is.null(having)) {
    assert_that(is.character(having), length(having) == 1L)
    out$having <- build_sql("HAVING ", escape(having, collapse = ", "))
  }

  if (!is.null(order_by)) {
    assert_that(is.character(order_by), length(order_by) > 0L)
    out$order_by <- build_sql("ORDER BY ", escape(order_by, collapse = ", "))
  }

  if (!is.null(limit)) {
    assert_that(is.integer(limit), length(limit) == 1L)
    out$limit <- build_sql("LIMIT ", limit)
  }

  if (!is.null(offset)) {
    assert_that(is.integer(offset), length(offset) == 1L)
    out$offset <- build_sql("OFFSET ", offset)
  }

  escape(compact(out), collapse = "\n", parens = FALSE)
}

var_names <- function(vars) {
  nms <- names2(vars)
  unname(ifelse(nms == "", vars, nms))
}
