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
#' # A "built"-in dataset
#' src_lahman()
#' 
#' # You can create a new postgres database at any location if you set 
#' # create = TRUE
#' new_db <- src_postgres(tempfile(), TRUE)
#' src_tbls(new_db)
src_postgres <- function(host = "", dbname = "", user = "", password = "", port = "", options = "", create = FALSE) {
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
  
  src("postgres", con = con, info = info, env = env)
}

#' @S3method format src_postgres
format.src_postgres <- function(x, ...) {
  paste0("<src_postgres> ", x$info$dbname, "\n", 
    wrap("tbls: ", paste0(src_tbls(x), collapse = ", ")))
}

#' @S3method src_tbls src_postgres
src_tbls.src_postgres <- function(x, ...) {
  dbListTables(x$con)
}

#' @S3method same_src src_postgres
same_src.src_postgres <- function(x, y) {
  if (!inherits(y, "src_postgres")) return(FALSE)
  identical(x$con, y$con)
}
