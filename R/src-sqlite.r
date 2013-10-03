#' Create a SQLite3 data source
#'
#' This data structure is basically a pointer to an SQLite3 database.
#'
#' @param path Path to SQLite database
#' @param create if \code{FALSE}, \code{path} must already exist. If
#'   \code{TRUE}, will create a new SQlite3 database at \code{path}.
#' @export
#' @examples
#' \dontrun{
#' my_db <- src_sqlite(db_path)
#' src_tbls(my_db)
#' }
#'
#' # A "built"-in dataset
#' lahman_sqlite()
#'
#' # You can create a new sqlite database at any location if you set
#' # create = TRUE
#' new_db <- src_sqlite(tempfile(), TRUE)
#' src_tbls(new_db)
src_sqlite <- function(path, create = FALSE) {
  if (create) {
    assert_that(!file.exists(path))
  } else {
    assert_that(is.readable(path))
  }

  if (!require("RSQLite")) {
    stop("RSQLite package required to connect to sqlite db", call. = FALSE)
  }
  if (!require("RSQLite.extfuns")) {
    stop("RSQLite.extfuns package required to effectively use sqlite db",
      call. = FALSE)
  }

  con <- dbi_connect(SQLite(), dbname = path)
  info <- db_info(con)

  src_sql("sqlite", con, path = path, info = info)
}

#' @S3method brief_desc src_sqlite
brief_desc.src_sqlite <- function(x) {
  paste0("sqlite ", x$info$serverVersion, " [", x$path, "]")
}

#' @S3method translate_env src_sqlite
translate_env.src_sqlite <- function(x) {
  sql_variant(
    sd = sql_prefix("stdev")
  )
}
