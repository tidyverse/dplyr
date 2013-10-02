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
#' @examples
#' l <- lahman_sqlite()
#' q <- query(l$con, "SELECT * FROM Batting WHERE YearID > 2000 AND TeamID = 'HOU'")
#' q$vars()
#' q$nrows()
#'
#' q$fetch(1)
#' all <- q$fetch(-1)
#'
#' q$show_sql()
#' q$explain_sql()
#'
#' # Use q$run() to run a query without retrieving results
query <- function(con, sql) {
  assert_that(is.string(sql))

  Query$new(con = con, sql = sql(sql), .vars = NULL, .res = NULL, .nrow = NULL)
}

#' @exportClass Query
Query <- setRefClass("Query",
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

    save_into = function(name = random_table_name()) {
      tt_sql <- build_sql("CREATE TEMPORARY TABLE ", ident(name), " AS ", sql)
      run_sql(con, tt_sql)

      name
    },

    from = function() {
      if (is.ident(sql)) {
        sql
      } else {
        build_sql("(", sql, ") AS master")
      }
    },

    vars = function() {
      if (!is.null(.vars)) return(.vars)

      no_rows <- build_sql("SELECT * FROM ", from(), " WHERE 1=0")

      .vars <<- qry_fields(con, no_rows)
      .vars
    },

    nrow = function() {
      if (!is.null(.nrow)) return(.nrow)

      rows <- build_sql("SELECT count(*) FROM ", from())
      .nrow <<- as.integer(qry_fetch(con, rows)[[1]])
      .nrow
    },

    ncol = function() {
      length(vars())
    }

  )
)
