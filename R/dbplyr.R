#' Database and SQL generics.
#'
#' The `sql_` generics are used to build the different types of SQL queries.
#' The default implementations in dbplyr generates ANSI 92 compliant SQL.
#' The `db_` generics execute actions on the database. The default
#' implementations in dbplyr typically just call the standard DBI S4
#' method.
#'
#' A few backend methods do not call the standard DBI S4 methods including
#'
#' * `db_data_type()`: Calls [DBI::dbDataType()] for every field
#'   (e.g. data frame column) and returns a vector of corresponding SQL data
#'   types
#'
#' * `db_save_query()`: Builds and executes a
#'   `CREATE [TEMPORARY] TABLE <table> ...` SQL command.
#'
#' * `db_create_index()`: Builds and executes a
#'   `CREATE INDEX <name> ON <table>` SQL command.
#'
#' * `db_drop_table()`: Builds and executes a
#'   `DROP TABLE [IF EXISTS]  <table>` SQL command.
#'
#' * `db_analyze()`: Builds and executes an
#'   `ANALYZE <table>` SQL command.
#'
#' Currently, [copy_to()] is the only user of `db_begin()`, `db_commit()`,
#' `db_rollback()`, `db_write_table()`, `db_create_indexes()`, `db_drop_table()` and
#' `db_analyze()`. If you find yourself overriding many of these
#' functions it may suggest that you should just override `copy_to()`
#' instead.
#'
#' `db_create_table()` and `db_insert_into()` have been deprecated
#' in favour of `db_write_table()`.
#'
#' @return Usually a logical value indicating success. Most failures should generate
#'  an error. However, `db_has_table()` should return `NA` if
#'  temporary tables cannot be listed with [DBI::dbListTables()] (due to backend
#'  API limitations for example). As a result, you methods will rely on the
#'  backend to throw an error if a table exists when it shouldn't.
#' @name backend_dbplyr
#' @param con A database connection.
#' @keywords internal
NULL

#' @name backend_dbplyr
#' @export
db_desc <- function(x) UseMethod("db_desc")

#' @name backend_dbplyr
#' @export
sql_translate_env <- function(con) UseMethod("sql_translate_env")

#' @name backend_dbplyr
#' @export
db_list_tables <- function(con) UseMethod("db_list_tables")

#' @name backend_dbplyr
#' @export
#' @param table A string, the table name.
db_has_table <- function(con, table) UseMethod("db_has_table")

#' @name backend_dbplyr
#' @export
#' @param fields A list of fields, as in a data frame.
db_data_type <- function(con, fields) UseMethod("db_data_type")
#' @export

#' @name backend_dbplyr
#' @export
db_save_query <- function(con, sql, name, temporary = TRUE, ...) {
  UseMethod("db_save_query")
}

#' @name backend_dbplyr
#' @export
db_begin <- function(con, ...) UseMethod("db_begin")

#' @name backend_dbplyr
#' @export
db_commit <- function(con, ...) UseMethod("db_commit")

#' @name backend_dbplyr
#' @export
db_rollback <- function(con, ...) UseMethod("db_rollback")

#' @name backend_dbplyr
#' @export
db_write_table <- function(con, table, types, values, temporary = FALSE, ...) {
  UseMethod("db_write_table")
}

#' @name backend_dbplyr
#' @export
db_create_table <- function(con, table, types, temporary = FALSE, ...) {
  UseMethod("db_create_table")
}

#' @name backend_dbplyr
#' @export
db_insert_into <- function(con, table, values, ...) {
  UseMethod("db_insert_into")
}

#' @name backend_dbplyr
#' @export
db_create_indexes <- function(con, table, indexes = NULL, unique = FALSE, ...) {
  UseMethod("db_create_indexes")
}

#' @name backend_dbplyr
#' @export
db_create_index <- function(con, table, columns, name = NULL, unique = FALSE,
                            ...) {
  UseMethod("db_create_index")
}

#' @name backend_dbplyr
#' @export
db_drop_table <- function(con, table, force = FALSE, ...) {
  UseMethod("db_drop_table")
}

#' @name backend_dbplyr
#' @export
db_analyze <- function(con, table, ...) UseMethod("db_analyze")

#' @export
#' @rdname backend_dbplyr
db_explain <- function(con, sql, ...) {
  UseMethod("db_explain")
}

#' @rdname backend_dbplyr
#' @export
db_query_fields <- function(con, sql, ...) {
  UseMethod("db_query_fields")
}

#' @rdname backend_dbplyr
#' @export
db_query_rows <- function(con, sql, ...) {
  UseMethod("db_query_rows")
}

#' @rdname backend_dbplyr
#' @export
sql_select <- function(con, select, from, where = NULL, group_by = NULL,
                       having = NULL, order_by = NULL, limit = NULL,
                       distinct = FALSE, ...) {
  UseMethod("sql_select")
}

#' @export
#' @rdname backend_dbplyr
sql_subquery <- function(con, from, name = random_table_name(), ...) {
  UseMethod("sql_subquery")
}

#' @rdname backend_dbplyr
#' @export
sql_join <- function(con, x, y, vars, type = "inner", by = NULL, ...) {
  UseMethod("sql_join")
}

#' @rdname backend_dbplyr
#' @export
sql_semi_join <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  UseMethod("sql_semi_join")
}

#' @rdname backend_dbplyr
#' @export
sql_set_op <- function(con, x, y, method) {
  UseMethod("sql_set_op")
}

#' @rdname backend_dbplyr
#' @export
sql_escape_string <- function(con, x) UseMethod("sql_escape_string")

#' @rdname backend_dbplyr
#' @export
sql_escape_ident <- function(con, x) UseMethod("sql_escape_ident")
