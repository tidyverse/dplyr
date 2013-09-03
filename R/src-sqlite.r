#' Create a SQLite data source
#' 
#' This data structure is basically a pointer to an SQLite database.
#' 
#' @param path Path to SQLite database
#' @export
#' @examples
#' db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
#' baseball <- src_sqlite(db_path)
#' src_tbls(baseball)
src_sqlite <- function(path) {
  assert_that(is.readable(path))
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
    "  tbls: ", paste0(src_tbls(x), collapse = ","))
}


#' @S3method src_tbls src_sqlite
src_tbls.src_sqlite <- function(x, ...) {
  dbListTables(x$con)  
}