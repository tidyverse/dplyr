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
                       having = NULL, order_by = NULL, limit = NULL,
                       distinct = FALSE, ...) {
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
    out$group_by <- build_sql(
      "GROUP BY ",
      escape(group_by, collapse = ", ", con = con)
    )
  }

  if (length(having) > 0L) {
    assert_that(is.character(having))
    out$having <- build_sql(
      "HAVING ",
      escape(having, collapse = ", ", con = con)
    )
  }

  if (length(order_by) > 0L) {
    assert_that(is.character(order_by))
    out$order_by <- build_sql(
      "ORDER BY ",
      escape(order_by, collapse = ", ", con = con)
    )
  }

  if (!is.null(limit) && !identical(limit, Inf)) {
    assert_that(is.numeric(limit), length(limit) == 1L, limit > 0)
    out$limit <- build_sql(
      "LIMIT ", sql(format(trunc(limit), scientific = FALSE)),
      con = con
    )
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
sql_join <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  UseMethod("sql_join")
}
#' @export
sql_join.default <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  JOIN <- switch(
    type,
    left = sql("LEFT JOIN"),
    inner = sql("INNER JOIN"),
    right = sql("RIGHT JOIN"),
    full = sql("FULL JOIN"),
    stop("Unknown join type:", type, call. = FALSE)
  )

  select <- sql_vector(c(
    sql_as(con, names(vars$x), vars$x, table = "TBL_LEFT"),
    sql_as(con, names(vars$y), vars$y, table = "TBL_RIGHT")
  ), collapse = ", ", parens = FALSE)

  on <- sql_vector(
    paste0(
      sql_table_prefix(con, by$x, "TBL_LEFT"),
      " = ",
      sql_table_prefix(con, by$y, "TBL_RIGHT")
    ),
    collapse = " AND ",
    parens = TRUE
  )

  # Wrap with SELECT since callers assume a valid query is returned
  build_sql(
    "SELECT ", select, "\n",
    "  FROM ", x, "\n",
    "  ", JOIN, " ", y, "\n",
    "  ON ", on, "\n",
    con = con
  )
}

sql_as <- function(con, var, alias = names(var), table = NULL) {
  if (length(var) == 0)
    return(ident())

  var <- sql_table_prefix(con, var, table = table)
  alias <- sql_escape_ident(con, alias)

  sql(paste0(var, " AS ", alias))
}

sql_table_prefix <- function(con, var, table = NULL) {
  var <- sql_escape_ident(con, var)

  if (!is.null(table)) {
    table <- sql_escape_ident(con, table)
    sql(paste0(table, ".", var))
  } else {
    var
  }

}

#' @export
sql_join.MySQLConnection <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  if (identical(type, "full")) {
    stop("MySQL does not support full joins", call. = FALSE)
  }
  NextMethod()
}

#' @rdname backend_sql
#' @export
sql_semi_join <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  UseMethod("sql_semi_join")
}
#' @export
sql_semi_join.default <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  # X and Y are subqueries named TBL_LEFT and TBL_RIGHT
  left <- escape(ident("TBL_LEFT"), con = con)
  right <- escape(ident("TBL_RIGHT"), con = con)
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
    "SELECT * FROM ", x, "\n\n",
    "WHERE ", if (anti) sql("NOT "), "EXISTS (\n",
    "  SELECT 1 FROM ", y, "\n",
    "  WHERE ", on, "\n",
    ")",
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
    "(", x, ")",
    "\n", sql(method), "\n",
    "(", y, ")"
  )
}

#' @export
sql_set_op.SQLiteConnection <- function(con, x, y, method) {
  # SQLite does not allow parentheses
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
