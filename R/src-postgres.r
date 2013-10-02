#' Create a postgres data source
#' 
#' This data structure represents a connection to an existing postgres database.
#' If you are running a local postgresql database, leave all parameters set as 
#' their defaults to connect. If you're connecting to a remote database, 
#' ask your database administrator for the values of these variables.
#' 
#' @param dbname Database
#' @param host,port Host name and port number of database
#' @param user,password User name and password (if needed)
#' @param options other additional options passed to command line client
#' @export
#' @examples
#' \dontrun{
#' my_db <- src_postgres()
#' src_tbls(my_db)
#' 
#' l <- src_postgres("lahman")
#' batting <- tbl(l, "Batting")
#' }
src_postgres <- function(dbname = "", host = "", port = "", user = "", 
                         password = "", options = "") {
  if (!require("RPostgreSQL")) {
    stop("RPostgreSQL package required to connect to postgres db", call. = FALSE)
  }

  con <- dbConnect(PostgreSQL(), host = host, dbname = dbname, user = user,
    password = password, port = port, options = options)
  info <- dbGetInfo(con)
  
  # Automatically disconnect database when it's collected by gc
  f <- function(x) {
    message("Auto-disconnecting postgres connection ", format(x$con))
    dbDisconnect(x$con)
  }
  environment(f) <- globalenv()  

  env <- new.env(parent = emptyenv())
  env$con <- con
  reg.finalizer(env, f)
  
  src_sql("postgres", con, info = info, env = env)
}

#' @S3method brief_desc src_postgres
brief_desc.src_postgres <- function(x) {
  info <- x$info
  host <- if (info$host == "") "localhost" else info$host
  
  paste0("postgres ", info$serverVersion, " [", info$user, "@", 
    host, ":", info$port, "/", info$dbname, "]")
}

#' @S3method translate_env src_postgres
translate_env.src_postgres <- function(x) {
  sql_variant(
    n = function() sql("count(*)"),
    # Extra aggregate functions
    cor = sql_prefix("corr"),
    cov = sql_prefix("covar_samp"),
    sd =  sql_prefix("stddev_samp"),
    var = sql_prefix("var_samp"),
    all = sql_prefix("bool_and"),
    any = sql_prefix("bool_or"),
    paste = function(x, collapse) build_sql("string_agg(", x, collapse, ")")
  )
}
