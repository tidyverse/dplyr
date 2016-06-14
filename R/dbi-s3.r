#' @import DBI
NULL


#' Source generics.
#'
#' These generics retrieve metadata for a given src.
#'
#' @keywords internal
#' @name backend_src
NULL

#' @name backend_src
#' @export
src_desc <- function(x) UseMethod("src_desc")

#' @name backend_src
#' @export
sql_translate_env <- function(con) UseMethod("sql_translate_env")

#' @name backend_src
#' @export
sql_translate_env.NULL <- function(con) {
  sql_variant(
    base_scalar,
    base_agg,
    base_win
  )
}

#' Database generics.
#'
#' These generics execute actions on the database. Most generics have a method
#' for \code{DBIConnection} which typically just call the standard DBI S4
#' method.
#'
#' Note, a few backend methods do not call the standard DBI S4 methods including
#' \itemize{
#' \item \code{db_data_type}: Calls DBI's \code{dbDataType} for every field
#' (e.g. data frame column) and returns a vector of corresponding SQL data
#' types
#' \item \code{db_save_query}: Builds and executes \code{CREATE [TEMPORARY]
#' TABLE <table> ...} SQL command.
#' \item \code{db_create_table}: Builds and executes \code{CREATE [TEMPORARY]
#' TABLE <table> ...} SQL command.
#' \item \code{db_create_index}: Builds and executes \code{CREATE INDEX <name>
#' ON <table>} SQL command.
#' \item \code{db_drop_table}: Builds and executes \code{DROP TABLE [IF EXISTS]
#'  <table>} SQL command.
#' \item \code{db_analyze}: Builds and executes \code{ANALYZE <table>} SQL
#' command.
#' \item \code{db_insert_into} and \code{db_explain}: do not have methods
#' calling corresponding DBI methods. The latter because no underlying DBI S4
#' method exists and the former because calls to the corresponding DBI S4
#' method (\code{dbWriteTable}) need to be able to specify an appropriate
#' combination of values for non-standard \code{append} and \code{overwrite}
#' arguments.
#' }
#'
#' Currently, \code{copy_to} is the only user of \code{db_begin()}, \code{db_commit()},
#' \code{db_rollback()}, \code{db_create_table()}, \code{db_insert_into()},
#' \code{db_create_indexes()}, \code{db_drop_table()} and
#' \code{db_analyze()}. If you find yourself overriding many of these
#' functions it may suggest that you should just override \code{\link{copy_to}}
#' instead.
#'
#' @return Usually a logical value indicating success. Most failures should generate
#'  an error. However, \code{db_has_table()} should return \code{NA} if
#'  temporary tables cannot be listed with \code{dbListTables} (due to backend
#'  API limitations for example). As a result, you methods will rely on the
#'  backend to throw an error if a table exists when it shouldn't.
#' @name backend_db
#' @param con A database connection.
#' @keywords internal
NULL

#' @name backend_db
#' @export
db_list_tables <- function(con) UseMethod("db_list_tables")
#' @export
db_list_tables.DBIConnection <- function(con) dbListTables(con)

#' @name backend_db
#' @export
#' @param table A string, the table name.
db_has_table <- function(con, table) UseMethod("db_has_table")
#' @export
db_has_table.DBIConnection <- function(con, table) dbExistsTable(con, table)

#' @name backend_db
#' @export
#' @param fields A list of fields, as in a data frame.
db_data_type <- function(con, fields) UseMethod("db_data_type")
#' @export
db_data_type.DBIConnection <- function(con, fields) {
  vapply(fields, dbDataType, dbObj = con, FUN.VALUE = character(1))
}

#' @name backend_db
#' @export
db_save_query <- function(con, sql, name, temporary = TRUE, ...) {
  UseMethod("db_save_query")
}

#' @export
db_save_query.DBIConnection <- function(con, sql, name, temporary = TRUE,
                                        ...) {
  tt_sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "),
    "TABLE ", ident(name), " AS ", sql, con = con)
  dbGetQuery(con, tt_sql)
  name
}

#' @name backend_db
#' @export
db_begin <- function(con, ...) UseMethod("db_begin")
#' @export
db_begin.DBIConnection <- function(con, ...) {
  dbBegin(con)
}

#' @name backend_db
#' @export
db_commit <- function(con, ...) UseMethod("db_commit")
#' @export
db_commit.DBIConnection <- function(con, ...) dbCommit(con)

#' @name backend_db
#' @export
db_rollback <- function(con, ...) UseMethod("db_rollback")
#' @export
db_rollback.DBIConnection <- function(con, ...) dbRollback(con)

#' @name backend_db
#' @export
db_create_table <- function(con, table, types, temporary = FALSE, ...) {
  UseMethod("db_create_table")
}
#' @export
db_create_table.DBIConnection <- function(con, table, types,
                                           temporary = FALSE, ...) {
  assert_that(is.string(table), is.character(types))

  field_names <- escape(ident(names(types)), collapse = NULL, con = con)
  fields <- sql_vector(paste0(field_names, " ", types), parens = TRUE,
    collapse = ", ", con = con)
  sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "),
    "TABLE ", ident(table), " ", fields, con = con)

  dbGetQuery(con, sql)
}

#' @name backend_db
#' @export
db_insert_into <- function(con, table, values, ...) {
  UseMethod("db_insert_into")
}

#' @name backend_db
#' @export
db_create_indexes <- function(con, table, indexes = NULL, unique = FALSE, ...) {
  UseMethod("db_create_indexes")
}

#' @export
db_create_indexes.DBIConnection <- function(con, table, indexes = NULL,
  unique = FALSE, ...) {
  if (is.null(indexes)) return()
  assert_that(is.list(indexes))

  for(index in indexes) {
    db_create_index(con, table, index, unique = unique, ...)
  }
}

#' @name backend_db
#' @export
db_create_index <- function(con, table, columns, name = NULL, unique = FALSE,
                            ...) {
  UseMethod("db_create_index")
}

#' @export
db_create_index.DBIConnection <- function(con, table, columns, name = NULL,
                                          unique = FALSE, ...) {
  assert_that(is.string(table), is.character(columns))

  name <- name %||% paste0(c(table, columns), collapse = "_")
  fields <- escape(ident(columns), parens = TRUE, con = con)
  sql <- build_sql(
    "CREATE ", if (unique) sql("UNIQUE "), "INDEX ", ident(name),
    " ON ", ident(table), " ", fields,
    con = con)

  dbGetQuery(con, sql)
}

#' @name backend_db
#' @export
db_drop_table <- function(con, table, force = FALSE, ...) {
  UseMethod("db_drop_table")
}
#' @export
db_drop_table.DBIConnection <- function(con, table, force = FALSE, ...) {
  sql <- build_sql("DROP TABLE ", if (force) sql("IF EXISTS "), ident(table),
    con = con)
  dbGetQuery(con, sql)
}

#' @name backend_db
#' @export
db_analyze <- function(con, table, ...) UseMethod("db_analyze")
#' @export
db_analyze.DBIConnection <- function(con, table, ...) {
  sql <- build_sql("ANALYZE ", ident(table), con = con)
  dbGetQuery(con, sql)
}

#' @export
#' @rdname backend_db
db_explain <- function(con, sql, ...) {
  UseMethod("db_explain")
}

#' @export
db_explain.DBIConnection <- function(con, sql, ...) {
  exsql <- build_sql("EXPLAIN ", sql, con = con)
  expl <- dbGetQuery(con, exsql)
  out <- utils::capture.output(print(expl))

  paste(out, collapse = "\n")
}

#' @rdname backend_db
#' @export
db_query_fields <- function(con, sql, ...) {
  UseMethod("db_query_fields")
}
#' @export
db_query_fields.DBIConnection <- function(con, sql, ...) {
  sql <- sql_select(con, sql("*"), sql_subquery(con, sql), where = sql("0 = 1"))
  qry <- dbSendQuery(con, sql)
  on.exit(dbClearResult(qry))

  res <- dbFetch(qry, 0)
  names(res)
}

#' @rdname backend_db
#' @export
db_query_rows <- function(con, sql, ...) {
  UseMethod("db_query_rows")
}
#' @export
db_query_rows.DBIConnection <- function(con, sql, ...) {
  from <- sql_subquery(con, sql, "master")
  rows <- build_sql("SELECT count(*) FROM ", from, con = con)

  as.integer(dbGetQuery(con, rows)[[1]])
}

# Utility functions ------------------------------------------------------------

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

# Creates an environment that disconnects the database when it's
# garbage collected
db_disconnector <- function(con, name, quiet = FALSE) {
  reg.finalizer(environment(), function(...) {
    if (!quiet) {
      message("Auto-disconnecting ", name, " connection ",
        "(", paste(con@Id, collapse = ", "), ")")
    }
    dbDisconnect(con)
  })
  environment()
}

res_warn_incomplete <- function(res, hint = "n = -1") {
  if (dbHasCompleted(res)) return()

  rows <- big_mark(dbGetRowCount(res))
  warning("Only first ", rows, " results retrieved. Use ", hint, " to retrieve all.",
    call. = FALSE)
}

