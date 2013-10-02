# An S3 shim on top of DBI.  The goal is to isolate all DBI calls into this
# file, so that when writing new connectors you can see all the existing
# code in one place, and hopefully remember the annoying DBI function names.
# 
# * db_ -> con = DBIConnection
# * qry_ -> con = DBIConnection, sql = string
# * res_ -> res = DBIResult
#
# This also makes it possible to shim over bugs in packages until they're 
# fixed upstream.

dbi_connect <- function(driver, ...) UseMethod("dbi_connect")
#' @S3method dbi_connect SQLiteDriver
dbi_connect.SQLiteDriver <- function(driver, ...) {
  con <- dbConnect(driver, ...)
  RSQLite.extfuns::init_extensions(con)
  con
}
#' @S3method dbi_connect DBIDriver
dbi_connect.DBIDriver <- function(driver, ...) {
  dbConnect(driver, ...)
}

# Database details -------------------------------------------------------------

db_info <- function(con) dbGetInfo(con)

db_list_tables <- function(con) UseMethod("db_list_tables")
#' @S3method db_list_tables DBIConnection
db_list_tables.DBIConnection <- function(con) dbListTables(con)
#' @S3method db_list_tables SQLiteConnection
db_list_tables.SQLiteConnection <- function(con) {
  sql <- "SELECT name FROM
    (SELECT * FROM sqlite_master UNION ALL
     SELECT * FROM sqlite_temp_master)
    WHERE type = 'table' OR type = 'view'
    ORDER BY name"
  qry_fetch(con, sql)[[1]]
}

db_data_type <- function(con, fields) {
  vapply(df, dbDataType, dbObj = con, FUN.VALUE = character(1))
}

# Query details ----------------------------------------------------------------

qry_fields <- function(con, sql) UseMethod("qry_fields")
#' @S3method qry_fields PostgreSQLConnection
qry_fields.PostgreSQLConnection <- function(con, sql) {
  qry <- dbSendQuery(con, sql)
  on.exit(dbClearResult(qry))
  
  dbGetInfo(qry)$fieldDescription[[1]]$name
}
#' @S3method qry_fields SQLiteConnection
qry_fields.SQLiteConnection <- function(con, sql) {
  names(qry_fetch(con, sql, 1L))
}

# Run a query, abandoning results
qry_run <- function(con, sql, data = NULL, in_transaction = FALSE, 
                    show = getOption("dplyr.show_sql"),
                    explain = getOption("dplyr.explain_sql")) {
  if (show) message(sql)
  if (explain) message(qry_explain(con, sql))
  
  if (in_transaction) {
    dbBeginTransaction(con)
    on.exit(dbCommit(con))
  }
  
  if (is.null(data)) {
    res <- dbSendQuery(con, sql)
  } else {
    res <- dbSendPreparedQuery(con, sql, bind.data = data)
  }
  dbClearResult(res)
  
  invisible(NULL)
}

# Run a query, fetching n results
qry_fetch <- function(con, sql, n = -1L, show = getOption("dplyr.show_sql"),
                      explain = getOption("dplyr.explain_sql")) {
  if (show) message(sql)
  if (explain) message(qry_explain(con, sql))
  
  res <- dbSendQuery(con, sql)
  on.exit(dbClearResult(res))
  
  out <- fetch(res, n)
  res_warn_incomplete(res)
  out
}

qry_fetch_paged <- function(con, sql, chunk_size, callback, 
                            show = getOption("dplyr.show_sql"),
                            explain = getOption("dplyr.explain_sql")) {
  if (show) message(sql)
  if (explain) message(qry_explain(con, sql))

  qry <- dbSendQuery(con, sql)
  on.exit(dbClearResult(qry))
  
  while (!dbHasCompleted(qry)) {
    chunk <- fetch(qry, chunk_size)
    callback(chunk)
  }
  
  invisible(TRUE)
}

qry_explain <- function(con, sql, ...) {
  UseMethod("qry_explain")
}
# http://sqlite.org/lang_explain.html
#' @S3method qry_explain SQLiteConnection
qry_explain.SQLiteConnection <- function(con, sql, ...) {
  exsql <- build_sql("EXPLAIN QUERY PLAN ", sql)
  expl <- qry_fetch(con, exsql, show = FALSE, explain = FALSE)
  rownames(expl) <- NULL
  out <- capture.output(print(expl))
  
  paste(out, collapse = "\n")
}
# http://www.postgresql.org/docs/9.3/static/sql-explain.html
#' @S3method qry_explain PostgreSQLConnection
qry_explain.PostgreSQLConnection <- function(con, sql, format = "text", ...) {
  format <- match.arg(format, c("text", "json", "yaml", "xml"))
  
  exsql <- build_sql("EXPLAIN ", 
    if (!is.null(format)) build_sql("(FORMAT ", sql(format), ") "), 
    sql)
  expl <- suppressWarnings(qry_fetch(con, exsql, show = FALSE, explain = FALSE))
  
  paste(expl[[1]], collapse = "\n")
}

qry_begin_trans <- function(con) UseMethod("qry_begin_trans")
#' @S3method qry_begin_trans SQLiteConnection
qry_begin_trans.SQLiteConnection <- function(con) dbBeginTransaction(con)
#' @S3method qry_begin_trans DBIConnection
qry_begin_trans.DBIConnection <- function(con) {
  dbGetQuery(con, "BEGIN TRANSACTION")
}

qry_commit <- function(con) dbCommit(con)

qry_rollback <- function(con) dbRollback(con)

# Result sets ------------------------------------------------------------------

res_warn_incomplete <- function(res) {
  if (dbHasCompleted(res)) return()
  
  rows <- formatC(dbGetRowCount(res), big.mark = ",")
  warning("Only first ", rows, " results retrieved. Use n = -1 to retrieve all.",
    call. = FALSE)
}
