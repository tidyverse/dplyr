# Environment for caching connections etc
cache <- new.env(parent = emptyenv())

db_location <- function(path = NULL, filename) {
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
