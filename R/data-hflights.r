#' Database versions of the hflights data
#'
#' These functions cache the data from the \code{hflights} database in
#' a local database, for use in examples and vignettes.
#'
#' @keywords internal
#' @name hflights_df
NULL

#' @export
#' @rdname hflights_df
#' @param path location of sqlite database file
hflights_sqlite <- function(path = NULL) {
  if (!is.null(cache$hflights)) return(cache$hflights_sqlite)

  path <- db_location(path, "hflights.sqlite")

  if (!file.exists(path)) {
    message("Caching hflights db at ", path)

    src <- src_sqlite(path, create = TRUE)
    copy_to(src, getExportedValue("hflights", "hflights"), "hflights",
      temporary = FALSE,
      indexes = list("Dest", c("Year", "Month", "DayofMonth"), "UniqueCarrier")
    )
  } else {
    src <- src_sqlite(path)
  }

  cache$hflights_sqlite <- src
  src
}

#' @export
#' @rdname hflights_df
#' @param dbname,... Arguments passed on to \code{\link{src_postgres}}
hflights_postgres <- function(dbname = "hflights", ...) {
  if (!is.null(cache$hflights_postgres)) return(cache$hflights_postgres)

  src <- src_postgres(dbname, ...)
  if (!db_has_table(src$con, "hflights")) {
    copy_to(src, getExportedValue("hflights", "hflights"), "hflights",
      temporary = FALSE,
      indexes = list("Dest", c("Year", "Month", "DayofMonth"), "UniqueCarrier")
    )
  }

  cache$hflights_postgres <- src
  src
}
