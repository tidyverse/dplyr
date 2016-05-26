#' A set of DBI methods to ease unit testing dplyr with DBI
#' @name src-test
#' @param con A database connection.
#' @param x Object to transform
#' @param sql A string containing an sql query.
#' @param ... Other arguments passed on to the individual methods
NULL

#' @export
#' @rdname src-test
db_query_fields.DBITestConnection <- function(con, sql, ...) {
  c("field1")
}

#' @export
#' @rdname src-test
sql_escape_ident.DBITestConnection <- function(con, x) {
  sql_quote(x, '`')
}

#' @export
#' @rdname src-test
sql_translate_env.DBITestConnection <- function(con) {
  dplyr::sql_variant(
    scalar = dplyr::sql_translator(.parent = dplyr::base_scalar),
    aggregate = dplyr::sql_translator(.parent = dplyr::base_agg),
    window = dplyr::sql_translator(.parent = dplyr::base_win)
  )
}
