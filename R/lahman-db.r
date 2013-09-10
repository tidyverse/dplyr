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
    
    ids <- as.list(names(df)[grepl("ID$", names(df))])
    copy_to(src, df, table, indexes = if (index) ids, temporary = FALSE)
  }
  
  invisible(TRUE)
}

#' @export
#' @rdname lahman_db
lahman <- function(path = NULL) {
  if (!is.null(con_cache$lahman)) return(con_cache$lahman)
  
  path <- db_location(path, "lahman.sqlite")

  if (!file.exists(path)) {
    message("Caching Lahman db at ", path)
    src <- src_sqlite(path, create = TRUE)
    lahman_db(src, quiet = TRUE)
  } else {
    src <- src_sqlite(path)
  }
  
  con_cache$lahman <- src    
  src
}

#' Create a database version of the hflights database
#' 
#' @inheritParams lahman
#' @export
#' @keywords internal
hflights_db <- function(path = NULL) {
  if (!is.null(con_cache$hflights)) return(con_cache$hflights)
  
  path <- db_location(path, "hflights.sqlite")
  
  if (!file.exists(path)) {
    message("Caching hflights db at ", path)
    
    src <- src_sqlite(path, create = TRUE)
    tbl <- copy_to(src, hflights, temporary = FALSE, 
      indexes = list("Dest", c("Year", "Month", "DayofMonth"), "UniqueCarrier"))
  } else {
    tbl <- tbl_sqlite(path, "hflights")
  }
  
  con_cache$hflights <- tbl    
  tbl
}

db_location <- function(path, filename) {
  if (!is.null(path)) {
    if (!is_writeable(path)) stop("Can not write to ", path, call. = FALSE)
    return(file.path(path, filename))
  }
  
  pkg <- file.path(system.file("db", package = "dplyr"))
  if (is_writeable(pkg)) return(file.path(pkg, filename))

  tmp <- tempdir()
  if (is_writeable(tmp)) return(file.path(tmp, filename))
  
  stop("Could not find writeable location to cache db", call. = FALSE)
}

is_writeable <- function(x) {
  unname(file.access(x, 2) == 0)
}

con_cache <- new.env(parent = emptyenv())
con_cache$lahman <- NULL