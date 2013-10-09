#' Cache and retrieve an \code{src_sqlite} of the Lahman baseball database.
#'
#' This creates an interesting database using data from the Lahman baseball
#' data source, provided by Sean Lahman at
#' \url{http://www.seanlahman.com/baseball-archive/statistics/}, and
#' made easily available in R through the \pkg{Lahman} package by
#' Michael Friendly, Dennis Murphy and Martin Monkman. See the documentation
#' for that package for documentation of the inidividual tables.
#'
#' @param path location to look for and cache SQLite database. If \code{NULL},
#'   the default, will first try storing in the installed package directory, and
#'   if that isn't writeable, a temporary directory.
#' @param ... Arguments passed to \code{src} on first
#'   load. For mysql and postgresql, the defaults assume you have a local
#'   server with \code{lahman} database already created. For bigquery, 
#'   it assumes you have read/write access to a project called 
#'   \code{Sys.getenv("BIGQUERY_PROJECT")}
#' @param type src type.
#' @examples
#' lahman_sqlite()
#' batting <- tbl(lahman_sqlite(), "Batting")
#' batting
#'
#' # Connect to a local postgres database with lahman database, if available
#' if (has_lahman("postgres")) {
#'   lahman_postgres()
#'   batting <- tbl(lahman_postgres(), "Batting")
#' }
#' @name lahman
NULL

#' @export
#' @rdname lahman
lahman_sqlite <- function(path = NULL) cache_lahman("sqlite", path = path)

#' @export
#' @rdname lahman
lahman_postgres <- function(...) cache_lahman("postgres", ...)

#' @export
#' @rdname lahman
lahman_mysql <- function(...) cache_lahman("mysql", ...)

#' @export
#' @rdname lahman
lahman_bigquery <- function(...) {
  if (!is.null(cache$lahman_bigquery)) return(cache$lahman_bigquery)
  
  src <- lahman_src("bigquery", ...)
  tables <- setdiff(lahman_tables(), src_tbls(src))
  
  jobs <- vector("list", length(tables))
  names(jobs) <- tables
  
  # Submit all upload jobs
  for(table in tables) {
    df <- get(table, "package:Lahman")
    
    if (!quiet) message("Creating table ", table)
    jobs[[table]] <- insert_upload_job(src$con$project, src$con$dataset, table, 
      df, billing = src$con$billing)
  }
  
  # Wait for all results
  all_ok <- TRUE
  for (table in names(jobs)) {
    message("Waiting for ", table)
    all_ok <- all_ok && succeeds(wait_for(jobs[[table]]))
  }

  if (!all_ok) stop("Load failed", call. = FALSE)
  
  cache$lahman_bigquery <- src
  src
}

cache_lahman <- function(type, ...) {
  if (!require("Lahman")) {
    stop("Please install the Lahman package", call. = FALSE)
  }
  
  cache_name <- paste0("lahman_", type)
  if (exists(cache_name, cache)) return(cache[[cache_name]])
  
  src <- lahman_src(type, ...)
  tables <- setdiff(lahman_tables(), src_tbls(src))
  
  # Create missing tables
  for(table in tables) {
    df <- get(table, "package:Lahman")
    message("Creating table: ", table)
    
    ids <- as.list(names(df)[grepl("ID$", names(df))])
    copy_to(src, df, table, indexes = ids, temporary = FALSE)
  }  
  
  cache[[cache_name]] <- src
  src
}

#' @rdname lahman
#' @export
has_lahman <- function(type, ...) {
  succeeds(lahman_src(type, ...), quiet = TRUE)
}

lahman_src <- function(type, ...) {
  switch(type,
    sqlite = src_sqlite(db_location(filename = "lahman.sqlite", ...), create = TRUE),
    mysql = src_mysql("lahman", ...),
    postgres = src_postgres("lahman", ...),
    bigquery = src_bigquery(Sys.getenv("BIGQUERY_PROJECT"), "lahman", ...),
    stop("Unknown src type ", type, call. = FALSE)
  )
}

succeeds <- function(x, quiet = FALSE) {
  ok <- FALSE
  try({
    force(x)
    ok <- TRUE
  }, silent = quiet)
  
  ok 
}

# Get list of all non-label data frames in package
lahman_tables <- function() {
  tables <- data(package = "Lahman")$results[, 3]
  tables[!grepl("Labels", tables)]
}
