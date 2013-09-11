#' Cache and retrieve an \code{src_sqlite} of the Lahman baseball database.
#' 
#' This creates an interesting database using data from the Lahman baseball
#' data source, provided by Sean Lahman at 
#' \url{http://www.seanlahman.com/baseball-archive/statistics/}, and
#' made easily available in R through the \pkg{Lahman} package by
#' Michael Friendly, Dennis Murphy and Martin Monkman. See the documentation
#' for that package for documentation of the inidividual tables.
#' 
#' @param path location to look for and cache database. If \code{NULL}, the 
#'   default, will first try storing in the installed package directory, and
#'   if that isn't writeable, a temporary directory.
#' @export
#' @examples
#' src_lahman()
#' batting <- tbl(src_lahman(), "Batting")
#' batting
src_lahman <- function(path = NULL) {
  if (!is.null(con_cache$lahman)) return(con_cache$lahman)
  
  path <- db_location(path, "lahman.sqlite")

  if (!file.exists(path)) {
    message("Caching Lahman db at ", path)
    src <- src_sqlite(path, create = TRUE)
    cache_lahman(src, quiet = TRUE)
  } else {
    src <- src_sqlite(path)
  }
  
  con_cache$lahman <- src    
  src
}
cache_lahman <- function(src, index = TRUE, quiet = FALSE) {
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

#' Houston flights data
#' 
#' This dataset contains all flights departing from Houston airports IAH
#' (George Bush Intercontinental) and HOU (Houston Hobby). The data comes
#' from the Research and Innovation Technology Administration at the 
#' Bureau of Transporation statistics:
#' \url{http://www.transtats.bts.gov/DatabaseInfo.asp?DB_ID=120&Link=0}
#' 
#' \code{src_hflights} caches a SQLite version of the data in a standard
#' location for use in examples.
#' 
#' @section Variables:
#' 
#' \itemize{
#'   \item \code{Year}, \code{Month}, \code{DayofMonth}: date of departure
#'   \item \code{DayOfWeek}: day of week of departure (useful for removing 
#'     weekend effects)
#'  \item \code{DepTime}, \code{ArrTime}: departure and arrival times 
#'    (in local time, hhmm)
#'  \item \code{UniqueCarrier}: unique abbreviation for a carrier
#'  \item \code{FlightNum}: flight number
#'  \item \code{TailNum}: airplane tail number
#'  \item \code{ActualElapsedTime}: elapsed time of flight, in minutes
#'  \item \code{AirTime}: flight time, in minutes
#'  \item \code{ArrDelay}, \code{DepDelay}: arrival and departure delays,
#'    in minutes
#'  \item \code{Origin}, \code{Dest} origin and destination airport codes
#'  \item \code{Distance}: distance of flight, in miles
#'  \item \code{TaxiIn}, \code{TaxiOut}: taxi in and out times in minutes
#'  \item \code{Cancelled}: cancelled indicator: 1 = Yes, 0 = No
#'  \item \code{CancellationCode}: reason for cancellation: A = carrier,
#'    B = weather, C = national air system, D = security
#'  \item \code{Diverted}: diverted indicator: 1 = Yes, 0 = No
#' }
#' @docType data
#' @name hflights
#' @usage hflights
#' @format A data frame with 227,496 rows and 21 columns.
#' @examples
#' head(hflights)
#' 
#' hflight_db <- tbl(src_hflights(), "hflights")
#' hflight_db
NULL

#' @inheritParams lahman
#' @export
#' @rdname hflights
src_hflights <- function(path = NULL) {
  if (!is.null(con_cache$hflights)) return(con_cache$hflights)
  
  path <- db_location(path, "hflights.sqlite")
  
  if (!file.exists(path)) {
    message("Caching hflights db at ", path)
    
    src <- src_sqlite(path, create = TRUE)
    copy_to(src, hflights, temporary = FALSE, 
      indexes = list("Dest", c("Year", "Month", "DayofMonth"), "UniqueCarrier"))
  } else {
    src <- src_sqlite(path)
  }
  
  con_cache$hflights <- src
  src
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