#' Cache and retrieve an \code{src_sqlite} of the Lahman baseball database.
#'
#' This creates an interesting database using data from the Lahman baseball
#' data source, provided by Sean Lahman at
#' \url{http://www.seanlahman.com/baseball-archive/statistics/}, and
#' made easily available in R through the \pkg{Lahman} package by
#' Michael Friendly, Dennis Murphy and Martin Monkman. See the documentation
#' for that package for documentation of the inidividual tables.
#'
#' @param ... Arguments passed to \code{src} on first
#'   load. For mysql and postgresql, the defaults assume you have a local
#'   server with \code{lahman} database already created. For bigquery,
#'   it assumes you have read/write access to a project called
#'   \code{Sys.getenv("BIGQUERY_PROJECT")}
#'
#'   For \code{lahman_srcs}, character vector of names giving srcs to generate.
#' @param quiet if \code{TRUE}, suppress messages about databases failing to
#'   connect.
#' @param type src type.
#' @examples
#' # Connect to a local sqlite database, if already created
#' if (require("RSQLite") && has_lahman("sqlite")) {
#'   lahman_sqlite()
#'   batting <- tbl(lahman_sqlite(), "Batting")
#'   batting
#' }
#'
#' # Connect to a local postgres database with lahman database, if available
#' if (require("RPostgreSQL") && has_lahman("postgres")) {
#'   lahman_postgres()
#'   batting <- tbl(lahman_postgres(), "Batting")
#' }
#' @name lahman
NULL

#' @export
#' @rdname lahman
lahman_sqlite <- function() {
  cache_lahman("sqlite", create = TRUE)
}

#' @export
#' @rdname lahman
lahman_postgres <- function(...) cache_lahman("postgres", ...)

#' @export
#' @rdname lahman
lahman_oracle <- function(...) cache_lahman("oracle", ...)

#' @export
#' @rdname lahman
lahman_mysql <- function(...) cache_lahman("mysql", ...)

#' @export
#' @rdname lahman
lahman_monetdb <- function(...) cache_lahman("monetdb", ...)

#' @export
#' @rdname lahman
lahman_df <- function() {
  src_df("Lahman")
}

#' @export
#' @rdname lahman
lahman_dt <- function() {
  src_dt("Lahman")
}

#' @export
#' @rdname lahman
lahman_bigquery <- function(...) {
  if (is_cached("lahman_bigquery")) return(get_cache("lahman_bigquery"))

  src <- lahman_src("bigquery", ...)
  tables <- setdiff(lahman_tables(), src_tbls(src))

  jobs <- vector("list", length(tables))
  names(jobs) <- tables

  # Submit all upload jobs
  for(table in tables) {
    df <- getExportedValue("Lahman", table)

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

  set_cache("lahman_bigquery", src)
}

cache_lahman <- function(type, ...) {
  cache_name <- paste0("lahman_", type)
  if (is_cached(cache_name)) return(get_cache(cache_name))

  src <- lahman_src(type, ...)
  tables <- setdiff(lahman_tables(), src_tbls(src))

  # Create missing tables
  for(table in tables) {
    df <- getExportedValue("Lahman", table)
    message("Creating table: ", table)

    ids <- as.list(names(df)[grepl("ID$", names(df))])
    copy_to(src, df, table, indexes = ids, temporary = FALSE)
  }

  set_cache(cache_name, src)
}

#' @rdname lahman
#' @export
has_lahman <- function(type, ...) {
  succeeds(lahman_src(type, ...), quiet = TRUE)
}

lahman_src <- function(type, ...) {
  switch(type,
    df = lahman_df(),
    dt = lahman_dt(),
    sqlite = src_sqlite(db_location(filename = "lahman.sqlite"), ...),
    mysql = src_mysql("lahman", ...),
    monetdb = src_monetdb("lahman", ...),
    postgres = src_postgres("lahman", ...),
    bigquery = src_bigquery(Sys.getenv("BIGQUERY_PROJECT"), "lahman", ...),
    oracle = src_oracle(NULL, "lahman", "lahman", ...),
    stop("Unknown src type ", type, call. = FALSE)
  )
}

#' @rdname lahman
#' @export
lahman_srcs <- function(..., quiet = NULL) {
  load_srcs(lahman_src, c(...), quiet = quiet)
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
