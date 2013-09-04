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
#' q$nrows()
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
  
  Query$new(con = con, sql = sql(sql), .vars = NULL, .res = NULL)
}

#' @exportClass Query
Query <- setRefClass("Query", 
  fields = c("con", "sql", ".vars", ".res"),
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
      expl <- fetch_sql_df(con, exsql)
      rownames(expl) <- NULL
      out <- capture.output(print(expl))
      
      message(paste(out, collapse = "\n"), "\n")
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
      if (getOption("dplyr.show_sql")) show_sql()
      if (getOption("dplyr.explain_sql")) explain_sql()
      
      run_sql(con, sql, in_transaction = in_transaction)
    },
    
    fetch_df = function(n = -1L) {
      if (getOption("dplyr.show_sql")) show_sql()
      if (getOption("dplyr.explain_sql")) explain_sql()
      
      fetch_sql_df(con, sql, n = n)
    },
    
    save_into = function(name = random_table_name()) {
      tt_sql <- build_sql("CREATE TEMPORARY TABLE ", ident(name), " AS ", sql)
      run_sql(con, tt_sql)
      
      name
    },
    
    vars = function() {
      if (length(.vars) > 0) return(.vars)
      
      no_rows <- build_sql("SELECT * FROM (", sql, ") WHERE 1=0")
      .vars <<- names(fetch_sql_df(con, no_rows))
      .vars
    },
    
    nrow = function() {
      rows <- build_sql("SELECT count(*) FROM (", sql, ")")
      fetch_sql_df(con, rows)[[1]]
    },
    
    ncol = function() {
      length(vars())
    }
    
  )
)

warn_incomplete <- function(qry, res) {
  if (dbHasCompleted(qry)) return()
  
  rows <- formatC(dbGetRowCount(qry), big.mark = ",")
  warning("Only first ", rows, " results retrieved. Use n = -1 to retrieve all.",
    call. = FALSE)
}

# Run a query, abandoning results
run_sql <- function(con, sql, in_transaction = FALSE) {
  if (in_transaction) dbBeginTransaction(con)
  
  qry <- dbSendQuery(con, sql)
  dbClearResult(qry)
  
  if (in_transaction) dbCommit(con)
  
  invisible(NULL)
}

# Run a query, fetching n results
fetch_sql_df <- function(con, sql, n = -1L) {
  qry <- dbSendQuery(con, sql)
  on.exit(dbClearResult(qry))
  
  res <- fetch(qry, n)
  warn_incomplete(qry)
  res
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