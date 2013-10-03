#' Connect to mysql/mariadb.
#' 
#' This data structure represents a connection to an existing mysql database.
#' If you are running a local mysqlql database, leave all parameters set as 
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
#' my_db <- src_mysql()
#' src_tbls(my_db)
#' 
#' l <- src_mysql("lahman")
#' batting <- tbl(l, "Batting")
#' }
src_mysql <- function(dbname, host = NULL, port = 0L, user = "root", 
  password = "", ...) {
  if (!require("RMySQL")) {
    stop("RMySQL package required to connect to mysql/mariadb", call. = FALSE)
  }
  
  con <- dbi_connect(MySQL(), dbname = dbname , host = host, port = port, 
    username = user, ...)
  info <- db_info(con)
  
  src_sql("mysql", con, 
    info = info, disco = db_disconnector(con, "mysql"))
}

#' @S3method brief_desc src_mysql
brief_desc.src_mysql <- function(x) {
  info <- x$info
  
  paste0("mysql ", info$serverVersion, " [", info$user, "@", 
    info$host, ":", info$port, "/", info$dbname, "]")
}

#' @S3method translate_env src_mysql
translate_env.src_mysql <- function(x) {
  sql_variant(
    n = function() sql("count(*)"),
    sd =  sql_prefix("stddev_samp"),
    var = sql_prefix("var_samp"),
    paste = function(x, collapse) build_sql("group_concat(", x, collapse, ")")
  )
}
