# Environment for caching connections etc
cache <- new.env(parent = emptyenv())

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

tmp_sqlite <- function() {
  if (is.null(cache$temp_sqlite_src)) {
    path <- tempfile(fileext = ".sqlite3")
    cache$temp_sqlite_src <- src_sqlite(path, create = TRUE)
  }

  cache$temp_sqlite_src
}

tmp_postgres <- function() {
  if (is.null(cache$temp_postgres_src)) {
    cache$temp_postgres_src <- src_postgres("test")
  }
  
  cache$temp_postgres_src
}

