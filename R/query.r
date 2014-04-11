#' Create a mutable query object.
#'
#' A query object is mutable wrapper around a \code{DBIResult} that caches
#' expensive operations, and insulates the rest of dplyr from the vagaries of
#' DBI and the individual database implementation.
#'
#' @keywords internal
#' @aliases Query-class
#' @param con a \code{DBOConnection}
#' @param sql a string containing an sql query.
#' @export
query <- function(con, sql, .vars) UseMethod("query")

#' @export
query.DBIConnection <- function(con, sql, .vars) {
  assert_that(is.string(sql))

  Query$new(con = con, sql = sql(sql), .vars = .vars, .res = NULL, .nrow = NULL)
}

#' @export
Query <- methods::setRefClass("Query",
  fields = c("con", "sql", ".vars", ".res", ".nrow"),
  methods = list(
    show = function() {
      cat("<Query> ", sql, "\n", sep = "")
      print(con)
    },

    # Currently, RSQLite supports only a single result set per connection
    # making holding on pointless, because it blocks the entire connection.

    run = function(data = NULL, in_transaction = FALSE) {
      qry_run(con, sql, data = data, in_transaction = in_transaction)
    },

    fetch = function(n = -1L) {
      qry_fetch(con, sql, n = n)
    },

    fetch_paged = function(chunk_size = 1e4, callback) {
      qry_fetch_paged(con, sql, chunk_size, callback)
    },

    save_into = function(name = random_table_name(), temporary = TRUE) {
      tt_sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "),
                          "TABLE ", ident(name), " AS ", sql, con = con)
      qry_run(con, tt_sql) 
      name
    },

    from = function() {
      if (is.ident(sql)) {
        sql
      } else {
        build_sql("(", sql, ") AS master", con = con)
      }
    },

    vars = function() {
      .vars
    },

    nrow = function() {
      if (!is.null(.nrow)) return(.nrow)

      rows <- build_sql("SELECT count(*) FROM ", from(), con = con)
      .nrow <<- as.integer(qry_fetch(con, rows)[[1]])
      .nrow
    },

    ncol = function() {
      length(vars())
    }
  )
)
