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
#' l <- src_lahman()
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
    
    run = function(data = NULL, in_transaction = FALSE) {
      run_sql(con, sql, data = data, in_transaction = in_transaction)
    },
        
    fetch_df = function(n = -1L) {
      fetch_sql_df(con, sql, n = n)
    },
    
    fetch_paged = function(chunk_size = 1e4, callback) {
      qry <- dbSendQuery(con, sql)
      on.exit(dbClearResult(qry))
      
      while (!dbHasCompleted(qry)) {
        chunk <- fetch(qry, chunk_size)
        callback(chunk)
      }
      
      invisible(TRUE)
    },
    
    save_into = function(name = random_table_name()) {
      tt_sql <- build_sql("CREATE TEMPORARY TABLE ", ident(name), " AS ", sql)
      run_sql(con, tt_sql)
      
      name
    },
    
    vars = function() {
      if (!is.null(.vars)) return(.vars)
      
      no_rows <- build_sql("SELECT * FROM (", sql, ") AS master WHERE 1=0")
      .vars <<- names(fetch_sql_df(con, no_rows))
      .vars
    },
    
    nrow = function() {
      if (!is.null(.nrow)) return(.nrow)
      
      rows <- build_sql("SELECT count(*) FROM (", sql, ") AS master")
      .nrow <<- fetch_sql_df(con, rows)[[1]]
      .nrow
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
run_sql <- function(con, sql, data = NULL, in_transaction = FALSE) {
  if (getOption("dplyr.show_sql")) message(sql)
  if (getOption("dplyr.explain_sql")) show_query_plan(con, sql)
  
  if (in_transaction) dbBeginTransaction(con)
  
  if (is.null(data)) {
    qry <- dbSendQuery(con, sql)
  } else {
    qry <- dbSendPreparedQuery(con, sql, bind.data = data)
  }
  dbClearResult(qry)
  
  if (in_transaction) dbCommit(con)
  
  invisible(NULL)
}

# Run a query, fetching n results
fetch_sql_df <- function(con, sql, n = -1L, show = getOption("dplyr.show_sql"), 
                         explain = getOption("dplyr.explain_sql")) {
  if (show) message(sql)
  if (explain) show_query_plan(con, sql)
  
  qry <- dbSendQuery(con, sql)
  on.exit(dbClearResult(qry))
  
  res <- fetch(qry, n)
  warn_incomplete(qry)
  res
}

show_query_plan <- function(con, sql) {
  exsql <- build_sql("EXPLAIN QUERY PLAN ", sql)
  expl <- fetch_sql_df(con, exsql, show = FALSE, explain = FALSE)
  rownames(expl) <- NULL
  out <- capture.output(print(expl))
  
  message(paste(out, collapse = "\n"), "\n")
}
