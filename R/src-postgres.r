#' Create a postgres data source
#' 
#' This data structure is basically a pointer to an postgres database.
#' 
#' @param path Path to postgres database
#' @param create if \code{FALSE}, \code{path} must already exist. If 
#'   \code{TRUE}, will create a new postgres3 database at \code{path}.
#' @export
#' @examples
#' \dontrun{
#' my_db <- src_postgres()
#' src_tbls(my_db)
#' }
#' 
#' src_postgres("lahman")
src_postgres <- function(dbname = "", host = "", user = "", password = "", port = "", options = "", create = FALSE) {
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
    n = function() sql("count(*)")
  )
}
