# Build an sql select query from a sql tbl.
sql_select <- function(x, select = NULL, where = NULL, order_by = NULL, ...,
                       n = -1L, explain = getOption("dplyr.explain_sql"),
                       show = getOption("dplyr.show_sql")) {
  assert_that(is.tbl(x))
  assert_that(is.numeric(n), length(n) == 1)

  select <- select %||% x$select %||% "*"
  where <- where %||% trans_sqlite(x$filter)
  order_by <- order_by %||% trans_sqlite(x$arrange)

  sql <- select_query(
    from = x$table,
    select = select,
    where = where,
    order_by = order_by,
    ...)

  exec_sql(x$con, sql, n = n, explain = explain, show = show)
}

#' Generate an SQL select query.
#'
#' Goal is to make valid sql given inputs - this knows nothing about sources.
#'
#' @keywords internal
#' @param select a character vector of fields to select. Names are used to
#'   create \code{AS} aliases.
#' @param from a string giving the table name
#' @param where if not \code{NULL}, a character vector of conditions, which
#'   will be combined with \code{AND}
#' @param group_by if not \code{NULL}, a character vector of named SQL
#'   expression used to group the data
#' @param having
#' @param order_by
#' @param limit
#' @param offset
#' @export
#' @examples
#' select_query("*", "mytable")
select_query <- function(select, from, where = NULL, group_by = NULL,
                         having = NULL, order_by = NULL, limit = NULL,
                         offset = NULL) {

  out <- vector("list", 8)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
    "limit", "offset")

  assert_that(is.character(select), length(select) > 0L)
  out$select <- paste0("SELECT ", sql_vars(select))

  assert_that(is.character(from), length(from) == 1L)
  out$from <- paste0("FROM ", escape_sql(from))

  if (length(where) > 0L) {
    assert_that(is.character(where))
    out$where <- paste0("WHERE ", paste0("(", where, ")", collapse = " AND "))
  }

  if (!is.null(group_by)) {
    assert_that(is.character(group_by), length(group_by) > 0L)
    out$group_by <- paste0("GROUP BY ", sql_vars(group_by))
  }

  if (!is.null(having)) {
    assert_that(is.character(having), length(having) == 1L)
    out$having <- paste0("HAVING ", having)
  }

  if (!is.null(order_by)) {
    assert_that(is.character(order_by), length(order_by) > 0L)
    out$order_by <- paste0("ORDER BY ", paste(order_by, collapse = ", "))
  }

  if (!is.null(limit)) {
    assert_that(is.integer(limit), length(limit) == 1L)
    out$limit <- paste0("LIMIT ", limit)
  }

  if (!is.null(offset)) {
    assert_that(is.integer(offset), length(offset) == 1L)
    out$offset <- paste0("OFFSET ", offset)
  }

  sql <- paste0(unlist(out, use.names = FALSE), collapse = "\n")
  paste0(sql, ";")
}

exec_sql <- function(con, sql, n = -1L, explain = FALSE, show = FALSE, fetch = TRUE) {
  assert_that(is.string(sql))

  if (isTRUE(explain)) {
    exsql <- paste0("EXPLAIN QUERY PLAN ", sql)
    out <- exec_sql(con, exsql, n = -1L, explain = FALSE, show = FALSE)
    rownames(out) <- NULL
    message(exsql)
    print(out)
    cat("\n")
  }

  if (isTRUE(show) && !isTRUE(explain)) {
    message(sql)
    cat("\n")
  }

  qry <- dbSendQuery(con, sql)
  on.exit(dbClearResult(qry))

  if (!fetch) return()

  res <- fetch(qry, n)
  if (!dbHasCompleted(qry)) {
    rows <- formatC(nrow(res), big.mark = ",")
    warning("Only first ", rows, " results retrieved. Use n = -1 to retrieve all.",
      call. = FALSE)
  }
  res
}

sql_keywords <- c(
  "ABORT", "ACTION", "ADD", "AFTER", "ALL", "ALTER", "ANALYZE", "AND", "AS",
  "ASC", "ATTACH", "AUTOINCREMENT", "BEFORE", "BEGIN", "BETWEEN", "BY",
  "CASCADE", "CASE", "CAST", "CHECK", "COLLATE", "COLUMN", "COMMIT", "CONFLICT",
  "CONSTRAINT", "CREATE", "CROSS", "CURRENT_DATE", "CURRENT_TIME",
  "CURRENT_TIMESTAMP", "DATABASE", "DEFAULT", "DEFERRABLE", "DEFERRED", "DELETE",
  "DESC", "DETACH", "DISTINCT", "DROP", "EACH", "ELSE", "END", "ESCAPE",
  "EXCEPT", "EXCLUSIVE", "EXISTS", "EXPLAIN", "FAIL", "FOR", "FOREIGN", "FROM",
  "FULL", "GLOB", "GROUP", "HAVING", "IF", "IGNORE", "IMMEDIATE", "IN", "INDEX",
  "INDEXED", "INITIALLY", "INNER", "INSERT", "INSTEAD", "INTERSECT", "INTO",
  "IS", "ISNULL", "JOIN", "KEY", "LEFT", "LIKE", "LIMIT", "MATCH", "NATURAL",
  "NO", "NOT", "NOTNULL", "NULL", "OF", "OFFSET", "ON", "OR", "ORDER", "OUTER",
  "PLAN", "PRAGMA", "PRIMARY", "QUERY", "RAISE", "REFERENCES", "REGEXP",
  "REINDEX", "RELEASE", "RENAME", "REPLACE", "RESTRICT", "RIGHT", "ROLLBACK",
  "ROW", "SAVEPOINT", "SELECT", "SET", "TABLE", "TEMP", "TEMPORARY", "THEN",
  "TO", "TRANSACTION", "TRIGGER", "UNION", "UNIQUE", "UPDATE", "USING", "VACUUM",
  "VALUES", "VIEW", "VIRTUAL", "WHEN", "WHERE")

sql_vars <- function(vars) {
  nms <- vapply(names2(vars), escape_sql, character(1))

  paste0(vars, ifelse(nms == "", "", " AS "), nms, collapse = ", ")
}

var_names <- function(vars) {
  nms <- vapply(names2(vars), escape_sql, character(1))
  unname(ifelse(nms == "", vars, nms))
}
