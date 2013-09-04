#' Create a SQLite3 data source
#' 
#' This data structure is basically a pointer to an SQLite3 database.
#' 
#' @param path Path to SQLite database
#' @param create if \code{FALSE}, \code{path} must already exist. If 
#'   \code{TRUE}, will create a new SQlite3 database at \code{path}.
#' @export
#' @examples
#' db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
#' baseball <- src_sqlite(db_path)
#' src_tbls(baseball)
#' 
#' # You can create a new sqlite database at any location if you set 
#' # create = TRUE
#' new_db <- src_sqlite(tempfile(), TRUE)
#' src_tbls("new_db")
src_sqlite <- function(path, create = FALSE) {
  if (create) {
    assert_that(!file.exists(path))
  } else {
    assert_that(is.readable(path))
  }
  
  if (!require("RSQLite")) {
    stop("RSQLite package required to connect to sqlite db", call. = FALSE)
  }
  if (!require("RSQLite.extfuns")) {
    stop("RSQLite.extfuns package required to effectively use sqlite db",
      call. = FALSE)
  }
  
  con <- dbConnect(SQLite(), dbname = path)
  RSQLite.extfuns::init_extensions(con)
  
  src("sqlite", con = con, path = path)
}

#' @S3method format src_sqlite
format.src_sqlite <- function(x, ...) {
  paste0("<src_sqlite> ", x$path, "\n", 
    wrap("tbls: ", paste0(src_tbls(x), collapse = ", ")))
}

#' @S3method src_tbls src_sqlite
src_tbls.src_sqlite <- function(x, ...) {
  sql <- "SELECT name FROM 
    (SELECT * FROM sqlite_master UNION ALL
     SELECT * FROM sqlite_temp_master)
    WHERE type = 'table' OR type = 'view'
    ORDER BY name"
  fetch_sql_df(x$con, sql)[[1]]
}