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
#' local <- c("df", "dt", "cpp")
#' db <- c("sqlite", "mysql", "postgres")
#' 
#' temp_srcs(local)
#' temp_srcs(db)
#' 
#' mtcars_tbls <- temp_tbls(local, mtcars)
#' }
temp_srcs <- function(...) {
  srcs <- c(...)
  setNames(lapply(srcs, temp_src), srcs)
}

temp_src <- function(type, ...) {
  cache_name <- paste("temp", type, "src", collapse = "-")
  if (!is.null(cache[[cache_name]])) {
    return(cache[[cache_name]])
  }
  
  env <- new.env(parent = emptyenv())  
  src <- switch(type,
    df =       src_df(env = env),
    dt =       src_dt(env = env),
    cpp =      src_cpp(env = env),
    sqlite =   src_sqlite(tempfile(), create = TRUE),
    mysql =    src_mysql("test", ...),
    postgres = src_postgres("test", ...),
    bigquery = src_bigquery(Sys.getenv("BIGQUERY_PROJECT"), "test", ...),
    stop("Unknown src type ", type, call. = FALSE)
  )
  
  cache[[cache_name]] <- src
  src
}

reset <- function(x) UseMethod("reset")
#' @S3method reset default
reset.default <- function(x) NULL
#' @S3method reset src_sql
reset.src_sql <- function(x) {
  for (tbl in src_tbls(x)) {
    dbRemoveTable(x$con, tbl)
  }
}

#' @rdname temp_srcs
temp_tbls <- function(srcs, df) {
  if (is.character(srcs)) {
    srcs <- temp_srcs(srcs)
  }
  
  lapply(srcs, copy_to, df, name = random_table_name())
}

