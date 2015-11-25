#' Database versions of the nycflights13 data
#'
#' These functions cache the data from the \code{nycflights13} database in
#' a local database, for use in examples and vignettes. Indexes are created
#' to making joining tables on natural keys efficient.
#'
#' @keywords internal
#' @name nycflights13
NULL

#' @export
#' @rdname nycflights13
#' @param path location of sqlite database file
nycflights13_sqlite <- function(path = NULL) {
  cache_computation("nycflights_sqlite", {
    path <- db_location(path, "nycflights13.sqlite")
    message("Caching nycflights db at ", path)
    src <- src_sqlite(path, create = TRUE)
    copy_nycflights13(src)
  })
}

#' @export
#' @rdname nycflights13
#' @param dbname,... Arguments passed on to \code{\link{src_postgres}}
nycflights13_postgres <- function(dbname = "nycflights13", ...) {
  cache_computation("nycflights_postgres", {
    message("Caching nycflights db in postgresql db ", dbname)
    copy_nycflights13(src_postgres(dbname, ...))
  })
}

#' @export
#' @rdname nycflights13
copy_nycflights13 <- function(src, ...) {
  all <- utils::data(package = "nycflights13")$results[, 3]
  unique_index <- list(
    airlines = list("carrier"),
    airports = list("faa"),
    planes =   list("tailnum"),
  )
  index <- list(
    flights =  list(c("year", "month", "day"), "carrier", "tailnum", "origin", "dest"),
    weather =  list(c("year", "month", "day"), "origin")
  )

  tables <- setdiff(all, src_tbls(src))

  # Create missing tables
  for(table in tables) {
    df <- getExportedValue("nycflights13", table)
    message("Creating table: ", table)

    copy_to(src, df, table, unique_indexes = unique_index[[table]],
            indexes = index[[table]], temporary = FALSE)
  }
  src
}
