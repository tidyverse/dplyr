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
#' l <- lahman()
#' q <- query(l$con, "SELECT * FROM Batting WHERE YearID > 2000 AND TeamID = 'HOU'")
#' q$vars()
#' 
#' q$fetch_df(1)
#' all <- q$fetch_df(-1)
#'
#' q$show_sql()
#' q$explain_sql()
#' 
#' # Use q$run() to run a query without retrieving results
query <- function(con, sql) {
  assert_that(is.string(sql))
  
  Query$new(con = con, sql = sql(sql))
}

#' @exportClass Query
Query <- setRefClass("Query", 
  fields = list(
    con = "DBIConnection",
    sql = "character",
    .vars = "character",
    .res = "ANY"
  ),
  methods = list(
    show = function() {
      cat("<Query> ", sql, "\n", sep = "")
      print(con)
    },
    
    show_sql = function() {
      message(sql, "\n")
    },
    
    explain_sql = function() {
      exsql <- build_sql("EXPLAIN QUERY PLAN ", sql)
      expl <- query(con, exsql)$fetch_df()
      rownames(expl) <- NULL
      out <- capture.output(print(expl))
      
      message(exsql, "\n", paste(out, collapse = "\n"), "\n")
    },
    
    # Currently, RSQLite supports only a single result set per connection
    # making holding on pointless, because it blocks the entire connection.
    
    # send = function() {
    #   .res <<- dbSendQuery(con, sql)
    # },
    # 
    # clear = function() {
    #   if (is.null(.res)) return()
    #   
    #   dbClearResult(.res)
    #   .res <<- NULL
    # },
    
    run = function(in_transaction = FALSE) {
      if (in_transaction) dbBeginTransaction(con)
      
      qry <- dbSendQuery(con, sql)
      dbClearResult(qry)

      if (in_transaction) dbCommit(con)
      
      invisible(NULL)
    },
    
    fetch_df = function(n = -1L) {
      qry <- dbSendQuery(con, sql)
      on.exit(dbClearResult(qry))
      
      res <- fetch(qry, n)
      warn_incomplete(qry)
      res
    },
    
    vars = function() {
      if (length(.vars) > 0) return(.vars)
      
      no_rows <- build_sql("SELECT * FROM (", sql, ") WHERE 1=0")
      .vars <<- names(query(con, no_rows)$fetch_df())
      .vars
    }    
    
  )
)

warn_incomplete <- function(qry, res) {
  if (dbHasCompleted(qry)) return()
  
  rows <- formatC(dbGetRowCount(qry), big.mark = ",")
  warning("Only first ", rows, " results retrieved. Use n = -1 to retrieve all.",
    call. = FALSE)
}

exec_sql <- function(con, sql, n = -1L, explain = FALSE, show = FALSE, fetch = TRUE) {
  q <- query(con, sql)
  assert_that(is.string(sql))
  
  if (isTRUE(show)) {
    q$show_sql()
  }
  if (isTRUE(explain)) {
    q$explain_sql()
  }
  
  if (fetch) {
    q$fetch_df(n = n)
  } else {
    q$run()
  }
}