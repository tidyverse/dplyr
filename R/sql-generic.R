#' SQL generation.
#'
#' These generics are used to run build various SQL queries. A default method
#' generates ANSI 92 compliant SQL, but variations in SQL across databases means
#' that it's likely that a backend will require at least a few methods.
#'
#' @return An SQL string.
#' @name backend_sql
#' @param con A database connection.
#' @keywords internal
NULL

#' @rdname backend_sql
#' @export
sql_select <- function(con, select, from, where = NULL, group_by = NULL,
                       having = NULL, order_by = NULL, distinct = FALSE, ...) {
  UseMethod("sql_select")
}
#' @export
sql_select.default <- function(con, select, from, where = NULL,
                               group_by = NULL, having = NULL,
                               order_by = NULL,
                               limit = NULL,
                               distinct = FALSE,
                               ...) {
  out <- vector("list", 7)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
    "limit")

  assert_that(is.character(select), length(select) > 0L)
  out$select <- build_sql(
    "SELECT ",
    if (distinct) sql("DISTINCT "),
    escape(select, collapse = ", ", con = con)
  )

  assert_that(is.character(from), length(from) == 1L)
  out$from <- build_sql("FROM ", from, con = con)

  if (length(where) > 0L) {
    assert_that(is.character(where))

    where_paren <- escape(where, parens = TRUE, con = con)
    out$where <- build_sql("WHERE ", sql_vector(where_paren, collapse = " AND "))
  }

  if (length(group_by) > 0L) {
    assert_that(is.character(group_by))
    out$group_by <- build_sql("GROUP BY ",
      escape(group_by, collapse = ", ", con = con))
  }

  if (length(having) > 0L) {
    assert_that(is.character(having))
    out$having <- build_sql("HAVING ",
      escape(having, collapse = ", ", con = con))
  }

  if (length(order_by) > 0L) {
    assert_that(is.character(order_by))
    out$order_by <- build_sql("ORDER BY ",
      escape(order_by, collapse = ", ", con = con))
  }

  if (!is.null(limit)) {
    assert_that(is.numeric(limit), length(limit) == 1L)
    out$limit <- build_sql("LIMIT ", limit, con = con)
  }

  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}

#' @export
#' @rdname backend_sql
sql_subquery <- function(con, from, name = random_table_name(), ...) {
  UseMethod("sql_subquery")
}
#' @export
sql_subquery.default <- function(con, from, name = unique_name(), ...) {
  if (is.ident(from)) {
    setNames(from, name)
  } else {
    build_sql("(", from, ") ", ident(name %||% random_table_name()), con = con)
  }
}

#' @rdname backend_sql
#' @export
sql_join <- function(con, x, y, type = "inner", by = NULL, ...) {
  UseMethod("sql_join")
}
#' @export
sql_join.default <- function(con, x, y, type = "inner", by = NULL, ...) {
  join <- switch(type,
    left = sql("LEFT"),
    inner = sql("INNER"),
    right = sql("RIGHT"),
    full = sql("FULL"),
    stop("Unknown join type:", type, call. = FALSE)
  )

  using <- all(by$x == by$y)

  if (using) {
    cond <- build_sql("USING ", lapply(by$x, ident), con = con)
  } else {
    on <- sql_vector(paste0(sql_escape_ident(con, by$x), " = ", sql_escape_ident(con, by$y)),
      collapse = " AND ", parens = TRUE)
    cond <- build_sql("ON ", on, con = con)
  }

  build_sql(
    'SELECT * FROM ',x, "\n\n",
    join, " JOIN\n\n" ,
    y, "\n\n",
    cond,
    con = con
  )
}

#' @rdname backend_sql
#' @export
sql_semi_join <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  UseMethod("sql_semi_join")
}
#' @export
sql_semi_join.default <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  # X and Y are subqueries named _LEFT and _RIGHT
  left <- escape(ident("_LEFT"), con = con)
  right <- escape(ident("_RIGHT"), con = con)
  on <- sql_vector(
    paste0(
      left,  ".", sql_escape_ident(con, by$x), " = ",
      right, ".", sql_escape_ident(con, by$y)
    ),
    collapse = " AND ",
    parens = TRUE,
    con = con
  )

  build_sql(
    'SELECT * FROM ', x, '\n\n',
    'WHERE ', if (anti) sql('NOT '), 'EXISTS (\n',
    '  SELECT 1 FROM ', y, '\n',
    '  WHERE ', on, '\n',
    ')',
    con = con
  )
}

#' @rdname backend_sql
#' @export
sql_set_op <- function(con, x, y, method) {
  UseMethod("sql_set_op")
}
#' @export
sql_set_op.default <- function(con, x, y, method) {
  build_sql(
    x,
    "\n", sql(method), "\n",
    y
  )
}

#' @rdname backend_sql
#' @export
sql_escape_string <- function(con, x) UseMethod("sql_escape_string")

#' @export
sql_escape_string.default <- function(con, x) {
  sql_quote(x, "'")
}

#' @rdname backend_sql
#' @export
sql_escape_ident <- function(con, x) UseMethod("sql_escape_ident")

#' @export
sql_escape_ident.default <- function(con, x) {
  sql_quote(x, '"')
}
