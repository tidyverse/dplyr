#' Create a database version of the Lahman baseball database.
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
lahman_db <- function(src) {
  if (!require("Lahman")) {
    stop("Please install the Lahman package", call. = FALSE)
  }
  
  # Get list of all non-label data frames in package
  tables <- data(package = "Lahman")$results[, 3]
  tables <- tables[!grepl("Labels", tables)]
  
  for(table in tables) {
    df <- get(table, "package:Lahman")
    message("Creating table ", table)
    tbl <- write_table(src, table, df)    
  }
  
  invisible(TRUE)
}