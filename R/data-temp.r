#' Connect to temporary data sources.
#'
#' These functions make it easy to take a local data frame and make available
#' as a tbl in every known src. All local srcs will work on any computer.
#' DBMS srcs will only currently work on Hadley's computer.
#'
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' local <- c("df", "dt")
#' db <- c("sqlite", "mysql", "postgres")
#'
#' temp_srcs(local)
#' temp_srcs(db)
#' }
temp_srcs <- function(..., quiet = NULL) {
  load_srcs(temp_src, c(...), quiet = quiet)
}

temp_src <- function(type, ...) {
  cache_name <- paste("temp", type, "src", collapse = "-")
  if (is_cached(cache_name)) return(get_cache(cache_name))

  env <- new.env(parent = emptyenv())
  src <- switch(type,
    df =       src_df(env = env),
    dt =       src_dt(env = env),
    sqlite =   src_sqlite(tempfile(), create = TRUE),
    mysql =    src_mysql("test", ...),
    postgres = src_postgres("test", ...),
    bigquery = src_bigquery(Sys.getenv("BIGQUERY_PROJECT"), "test", ...),
    stop("Unknown src type ", type, call. = FALSE)
  )

  set_cache(cache_name, src)
}

reset <- function(x) UseMethod("reset")
#' @export
reset.default <- function(x) NULL
#' @export
reset.src_sql <- function(x) {
  for (tbl in src_tbls(x)) {
    dbRemoveTable(x$con, tbl)
  }
}
#' @export
reset.list <- function(x) {
  for (y in x) reset(y)
}

#' @rdname temp_srcs
temp_load <- function(srcs, df, name = NULL) {
  if (is.character(srcs)) {
    srcs <- temp_srcs(srcs)
  }

  if (is.data.frame(df)) {
    if (is.null(name)) name <- random_table_name()
    lapply(srcs, copy_to, df, name = name)
  } else {
    if (is.null(name)) {
      name <- replicate(length(df), random_table_name())
    } else {
      stopifnot(length(name) == length(df))
    }

    lapply(srcs, function(x) {
      Map(function(df, name) copy_to(x, df, name), df, name)
    })
  }
}

