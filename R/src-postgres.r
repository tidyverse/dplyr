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
#' @param create if \code{FALSE}, \code{dbname} must already exist. If 
#'   \code{TRUE}, will create a new postgres - to do this you must either
#'   be a super user or have the CREATE DB permssion.
#' @export
#' @examples
#' \dontrun{
#' my_db <- src_postgres()
#' src_tbls(my_db)
#' 
#' src_postgres("lahman")
#' }
src_postgres <- function(dbname = "", host = "", port = "", user = "", 
                         password = "", options = "", create = FALSE) {
  if (!require("RPostgreSQL")) {
    stop("RPostgreSQL package required to connect to postgres db", call. = FALSE)
  }
# 
#   if (create) {
#   } else {
#   }
#   

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

#' @S3method format src_postgres
format.src_postgres <- function(x, ...) {
  paste0("<src_postgres> ", x$info$dbname, "\n", 
    wrap("tbls: ", paste0(src_tbls(x), collapse = ", ")))
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
