#' Create a mutable query object.
#'
#' A query object is mutable wrapper around a \code{DBIResult} that caches
#' expensive operations, and insulates the rest of dplyr from the vagaries of
#' DBI and the individual database implementation.
#'
#' @keywords internal
#' @param con a \code{DBOConnection}
#' @param sql a string containing an sql query.
#' @export
query <- function(con, sql, .vars) UseMethod("query")

#' @export
query.DBIConnection <- function(con, sql, .vars) {
  assert_that(is.string(sql))

  Query$new(con, sql(sql), .vars)
}

Query <- R6::R6Class("Query",
  private = list(
    .nrow = NULL,
    .vars = NULL
  ),
  public = list(
    con = NULL,
    sql = NULL,

    initialize = function(con, sql, vars) {
      self$con <- con
      self$sql <- sql
      private$.vars <- vars
    },

    print = function(...) {
      cat("<Query> ", self$sql, "\n", sep = "")
      print(self$con)
    },

    fetch = function(n = -1L) {
      res <- dbSendQuery(self$con, self$sql)
      on.exit(dbClearResult(res))

      out <- fetch(res, n)
      res_warn_incomplete(res)
      out
    },

    fetch_paged = function(chunk_size = 1e4, callback) {
      qry <- dbSendQuery(self$con, self$sql)
      on.exit(dbClearResult(qry))

      while (!dbHasCompleted(qry)) {
        chunk <- fetch(qry, chunk_size)
        callback(chunk)
      }

      invisible(TRUE)
    },

    vars = function() {
      private$.vars
    },

    nrow = function() {
      if (!is.null(private$.nrow)) return(private$.nrow)
      private$.nrow <- db_query_rows(self$con, self$sql)
      private$.nrow
    },

    ncol = function() {
      length(self$vars())
    }
  )
)
