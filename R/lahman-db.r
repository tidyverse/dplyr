#' Create a database version of the Lahman baseball database.
#' 
#' \code{lahman_db} creates a copy of the Lahman baseball database in the 
#' database src of your choice. \code{lahman} creates a cached version of the
#' Lahman database in a standard location for use in examples.
#' 
#' This creates an interesting database using data from the Lahman baseball
#' data source, provided by Sean Lahman at 
#' \url{http://www.seanlahman.com/baseball-archive/statistics/}, and
#' made easily available in R through the \pkg{Lahman} package by
#' Michael Friendly, Dennis Murphy and Martin Monkman.
#' 
#' @param src a data source with write access
#' @export
#' @examples
#' \dontrun{
#' db <- src_sqlite("~/desktop/lahman.sqlite", create = TRUE)
#' lahman_db(db)
#' }
lahman_db <- function(src, index = TRUE, quiet = FALSE) {
  if (!require("Lahman")) {
    stop("Please install the Lahman package", call. = FALSE)
  }
  
  # Get list of all non-label data frames in package
  tables <- data(package = "Lahman")$results[, 3]
  tables <- tables[!grepl("Labels", tables)]
  
  for(table in tables) {
    df <- get(table, "package:Lahman")
    if (!quiet) message("Creating table ", table)
    tbl <- write_table(src, table, df)   
    
    if (index) {
      ids <- names(df)[grepl("ID$", names(df))]
      for (id in ids) {
        create_index(tbl, id)
      }
    }
  }
  
  invisible(TRUE)
}

#' @export
#' @rdname lahman_db
lahman <- function(path = NULL) {
  path <- path %||% file.path(system.file("db", package = "dplyr"), "lahman.db")
  if (file.exists(path)) {
    return(src_sqlite(path))
  }
  
  message("Caching Lahman db at ", path)
  src <- src_sqlite(path, create = TRUE)
  lahman_db(src, quiet = TRUE)
  
  src
}
