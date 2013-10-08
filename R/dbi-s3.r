# An S3 shim on top of DBI.  The goal is to isolate all DBI calls into this
# file, so that when writing new connectors you can see all the existing
# code in one place, and hopefully remember the annoying DBI function names.
# 
# * db_ -> con = DBIConnection
# * qry_ -> con = DBIConnection, sql = string
# * res_ -> res = DBIResult
# * sql_ -> con = DBIConnection, table = string, ...
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
#' @S3method db_list_tables bigquery
db_list_tables.bigquery <- function(con) {
  list_tables(con$project, con$dataset)
}

db_has_table <- function(con, table) UseMethod("db_has_table")
#' @S3method db_has_table default
db_has_table.default <- function(con, table) {
  table %in% db_list_tables(con)
}
#' @S3method db_has_table MySQLConnection
db_has_table.MySQLConnection <- function(con, table) {
  # MySQL has no way to list temporary tables, so we always return TRUE to
  # skip any local checks and rely on the database to throw informative errors
  TRUE
}


db_data_type <- function(con, fields) UseMethod("db_data_type")

#' @S3method db_data_type DBIConnection
db_data_type.DBIConnection <- function(con, fields) {
  vapply(fields, dbDataType, dbObj = con, FUN.VALUE = character(1))
}

#' @S3method db_data_type MySQLConnection
db_data_type.MySQLConnection <- function(con, fields) {
  char_type <- function(x) {
    n <- max(nchar(as.character(x), "bytes")) 
    if (n <= 65535) {
      paste0("varchar(", n, ")")
    } else {
      "mediumtext"
    }
  }
  
  data_type <- function(x) {
    switch(class(x)[1],
      logical = "boolean",
      integer = "integer",
      numeric = "double",
      factor =  char_type(x),
      character = char_type(x),
      Date =    "date",
      POSIXct = "datetime",
      stop("Unknown class ", paste(class(x), collapse = "/"), call. = FALSE)
    )
  }
  vapply(fields, data_type, character(1))  
}

# Creates an environment that disconnects the database when it's
# garbage collected
db_disconnector <- function(con, name, quiet = FALSE) {
  DbDisconnector$new(con = con, name = name, quiet = quiet)
}
DbDisconnector <- setRefClass("DbDisconnector", 
  fields = c("con", "name", "quiet"),
  methods = list(
    finalize = function() {
      if (!quiet) {
        message("Auto-disconnecting ", name, " connection ", 
          "(", paste(con@Id, collapse = ", "), ")")
      }
      dbDisconnect(con)
    }
  )
)

# Query details ----------------------------------------------------------------

qry_fields <- function(con, sql) UseMethod("qry_fields")

#' @S3method qry_fields DBIConnection
qry_fields.DBIConnection <- function(con, sql) {
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
  UseMethod("qry_fetch")
}

#' @S3method qry_fetch DBIConnection
qry_fetch.DBIConnection <- function(con, sql, n = -1L, 
                                    show = getOption("dplyr.show_sql"),
                                    explain = getOption("dplyr.explain_sql")) {
  
  if (show) message(sql)
  if (explain) message(qry_explain(con, sql))
  
  res <- dbSendQuery(con, sql)
  on.exit(dbClearResult(res))
  
  out <- fetch(res, n)
  res_warn_incomplete(res)
  out
}

#' @S3method qry_fetch bigquery
qry_fetch.bigquery <- function(con, sql, n = -1L, 
                                    show = getOption("dplyr.show_sql"),
                                    explain = getOption("dplyr.explain_sql")) {
  
  if (show) message(sql)
  
  if (identical(n, -1L)) {
    max_pages <- Inf
  } else {
    max_pages <- ceil(n / 1e4)
  }

  query_exec(con$project, con$dataset, sql, max_pages = max_pages, 
    page_size = 1e4)
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

# Result sets ------------------------------------------------------------------

res_warn_incomplete <- function(res) {
  if (dbHasCompleted(res)) return()
  
  rows <- formatC(dbGetRowCount(res), big.mark = ",")
  warning("Only first ", rows, " results retrieved. Use n = -1 to retrieve all.",
    call. = FALSE)
}

# SQL queries ------------------------------------------------------------------


sql_begin_trans <- function(con) UseMethod("sql_begin_trans")
#' @S3method sql_begin_trans SQLiteConnection
sql_begin_trans.SQLiteConnection <- function(con) dbBeginTransaction(con)
#' @S3method sql_begin_trans DBIConnection
sql_begin_trans.DBIConnection <- function(con) {
  qry_run(con, "BEGIN TRANSACTION")
}
#' @S3method sql_begin_trans DBIConnection
sql_begin_trans.MySQLConnection <- function(con) {
  qry_run(con, "START TRANSACTION")
}

sql_commit <- function(con) UseMethod("sql_commit")
#' @S3method sql_commit DBIConnection
sql_commit.DBIConnection <- function(con) dbCommit(con)
#' @S3method sql_commit MySQLConnection
sql_commit.MySQLConnection <- function(con) {
  qry_run(con, "COMMIT")
}

sql_rollback <- function(con) dbRollback(con)

sql_create_table <- function(con, table, types, temporary = FALSE) {
  assert_that(is.string(table), is.character(types))
  
  field_names <- escape(ident(names(types)), collapse = NULL, con = con)
  fields <- sql_vector(paste0(field_names, " ", types), parens = TRUE, 
    collapse = ", ", con = con)
  sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "), 
    "TABLE ", ident(table), " ", fields, con = con)

  qry_run(con, sql)
}

sql_insert_into <- function(con, table, values) {
  UseMethod("sql_insert_into")
}

#' @S3method sql_insert_into SQLiteConnection
sql_insert_into.SQLiteConnection <- function(con, table, values) {
  params <- paste(rep("?", ncol(values)), collapse = ", ")
  
  sql <- build_sql("INSERT INTO ", table, " VALUES (", sql(params), ")")
  qry_run(con, sql, data = values)
}

#' @S3method sql_insert_into PostgreSQLConnection
sql_insert_into.PostgreSQLConnection <- function(con, table, values) {
  cols <- lapply(values, escape, collapse = NULL, parens = FALSE, con = con)
  col_mat <- matrix(unlist(cols, use.names = FALSE), nrow = nrow(values))
  
  rows <- apply(col_mat, 1, paste0, collapse = ", ")
  values <- paste0("(", rows, ")", collapse = "\n, ")
  
  sql <- build_sql("INSERT INTO ", ident(table), " VALUES ", sql(values)) 
  qry_run(con, sql)
}

#' @S3method sql_insert_into MySQLConnection
sql_insert_into.MySQLConnection <- function(con, table, values) {
  
  # Convert factors to strings
  is_factor <- vapply(values, is.factor, logical(1))
  values[is_factor] <- lapply(values[is_factor], as.character)
  
  # Encode special characters in strings
  is_char <- vapply(values, is.character, logical(1))
  values[is_char] <- lapply(values[is_char], encodeString)
  
  tmp <- tempfile(fileext = ".csv")
  write.table(values, tmp, sep = "\t", quote = FALSE, qmethod = "escape",
    row.names = FALSE, col.names = FALSE)
  
  sql <- build_sql("LOAD DATA LOCAL INFILE ", tmp, " INTO TABLE ", ident(table), 
    con = con)
  qry_run(con, sql)

  invisible()
}


sql_create_indexes <- function(con, table, indexes = NULL, ...) {
  UseMethod("sql_create_indexes")
}

#' @S3method sql_create_indexes DBIConnection
sql_create_indexes.DBIConnection <- function(con, table, indexes = NULL, ...) {
  if (is.null(indexes)) return()
  assert_that(is.list(indexes))
  
  for(index in indexes) {
    sql_create_index(con, table, index, ...)
  }
}

#' @S3method sql_create_indexes MySQLConnection
sql_create_indexes.MySQLConnection <- function(con, table, indexes = NULL, ...) {
  sql_add_index <- function(columns) {
    name <- paste0(c(table, columns), collapse = "_")
    fields <- escape(ident(columns), parens = TRUE, con = con)
    build_sql("ADD INDEX ", ident(name), " ", fields, con = con)
  }
  add_index <- sql(vapply(indexes, sql_add_index, character(1)))
  sql <- build_sql("ALTER TABLE ", ident(table), "\n",  
    escape(add_index, collapse = ",\n"), con = con)
  qry_run(con, sql)
}

sql_create_index <- function(con, table, columns, name = NULL, unique = FALSE) {
  assert_that(is.string(table), is.character(columns))
  
  name <- name %||% paste0(c(table, columns), collapse = "_")
  
  fields <- escape(ident(columns), parens = TRUE, con = con)
  sql <- build_sql("CREATE ", if (unique) sql("UNIQUE "), "INDEX ", ident(name), 
    " ON ", ident(table), " ", fields, con = con)
  
  qry_run(con, sql)
}

sql_drop_table <- function(con, table, force = FALSE) {
  sql <- build_sql("DROP TABLE ", if (force) sql("IF EXISTS "), ident(table), 
    con = con)
  qry_run(con, sql)
}

sql_analyze <- function(con, table) UseMethod("sql_analyze")

#' @S3method sql_analyze DBIConnection
sql_analyze.DBIConnection <- function(con, table) {
  sql <- build_sql("ANALYZE ", ident(table), con = con)
  qry_run(con, sql)
}

#' @S3method sql_analyze MySQLConnection
sql_analyze.MySQLConnection <- function(con, table) {
  sql <- build_sql("ANALYZE TABLE", ident(table), con = con)
  qry_run(con, sql)
}

sql_select <- function(con, select, from, where = NULL, group_by = NULL,
                       having = NULL, order_by = NULL, limit = NULL, 
                       offset = NULL) {
  
  out <- vector("list", 8)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
    "limit", "offset")
  
  assert_that(is.character(select), length(select) > 0L)
  out$select <- build_sql("SELECT ", escape(select, collapse = ", ", con = con))
  
  assert_that(is.character(from), length(from) == 1L)
  out$from <- build_sql("FROM ", from, con = con)
  
  if (length(where) > 0L) {
    assert_that(is.character(where))
    out$where <- build_sql("WHERE ", 
      escape(where, collapse = " AND ", con = con))
  }
  
  if (!is.null(group_by)) {
    assert_that(is.character(group_by), length(group_by) > 0L)
    out$group_by <- build_sql("GROUP BY ", 
      escape(group_by, collapse = ", ", con = con))
  }
  
  if (!is.null(having)) {
    assert_that(is.character(having), length(having) == 1L)
    out$having <- build_sql("HAVING ", 
      escape(having, collapse = ", ", con = con))
  }
  
  if (!is.null(order_by)) {
    assert_that(is.character(order_by), length(order_by) > 0L)
    out$order_by <- build_sql("ORDER BY ", 
      escape(order_by, collapse = ", ", con = con))
  }
  
  if (!is.null(limit)) {
    assert_that(is.integer(limit), length(limit) == 1L)
    out$limit <- build_sql("LIMIT ", limit, con = con)
  }
  
  if (!is.null(offset)) {
    assert_that(is.integer(offset), length(offset) == 1L)
    out$offset <- build_sql("OFFSET ", offset, con = con)
  }
  
  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}

# Utility functions ------------------------------------------------------------

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}
