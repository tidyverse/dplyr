#' Cache and retrieve an \code{src_sqlite} of the Lahman baseball database.
#'
#' This creates an interesting database using data from the Lahman baseball
#' data source, provided by Sean Lahman at
#' \url{http://www.seanlahman.com/baseball-archive/statistics/}, and
#' made easily available in R through the \pkg{Lahman} package by
#' Michael Friendly, Dennis Murphy and Martin Monkman. See the documentation
#' for that package for documentation of the inidividual tables.
#'
#' @param ... Other arguments passed to \code{src} on first
#'   load. For mysql and postgresql, the defaults assume you have a local
#'   server with \code{lahman} database already created.
#'   For \code{lahman_srcs}, character vector of names giving srcs to generate.
#' @param quiet if \code{TRUE}, suppress messages about databases failing to
#'   connect.
#' @param type src type.
#' @keywords internal
#' @examples
#' # Connect to a local sqlite database, if already created
#' if (has_lahman("sqlite")) {
#'   lahman_sqlite()
#'   batting <- tbl(lahman_sqlite(), "Batting")
#'   batting
#' }
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
lahman_sqlite <- function(path = NULL) {
  path <- db_location(path, "lahman.sqlite")
  copy_lahman(src_sqlite(path = path, create = TRUE))
}

#' @export
#' @rdname lahman
lahman_postgres <- function(dbname = "lahman", ...) {
  copy_lahman(src_postgres(dbname, ...))
}

#' @export
#' @rdname lahman
lahman_mysql <- function(dbname = "lahman", ...) {
  copy_lahman(src_mysql(dbname, ...))
}

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

#' @rdname lahman
#' @export
copy_lahman <- function(src, ...) {
  # Create missing tables
  tables <- setdiff(lahman_tables(), src_tbls(src))
  for(table in tables) {
    df <- getExportedValue("Lahman", table)
    message("Creating table: ", table)

    ids <- as.list(names(df)[grepl("ID$", names(df))])
    copy_to(src, df, table, indexes = ids, temporary = FALSE)
  }

  src
}
# Get list of all non-label data frames in package
lahman_tables <- function() {
  tables <- data(package = "Lahman")$results[, 3]
  tables[!grepl("Labels", tables)]
}

#' @rdname lahman
#' @export
has_lahman <- function(type, ...) {
  if (!requireNamespace("Lahman", quietly = TRUE)) return(FALSE)
  if (missing(type)) return(TRUE)

  succeeds(lahman(type, ...), quiet = TRUE)
}

#' @rdname lahman
#' @export
lahman_srcs <- function(..., quiet = NULL) {
  load_srcs(lahman, c(...), quiet = quiet)
}

lahman <- function(type, ...) {
  if (missing(type)) {
    src_df("Lahman")
  } else {
    f <- match.fun(paste0("lahman_", type))
    f(...)
  }
}
