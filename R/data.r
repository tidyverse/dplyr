# Environment for caching connections etc
cache <- new.env(parent = emptyenv())

is_cached <- function(name) exists(name, envir = cache)
set_cache <- function(name, value) {
#   message("Setting ", name, " in cache")
  assign(name, value, envir = cache)
  value
}
get_cache <- function(name) {
#   message("Getting ", name, " from cache")
  get(name, envir = cache)
}

load_srcs <- function(f, src_names, quiet = NULL) {
  if (is.null(quiet)) {
    quiet <- !identical(Sys.getenv("NOT_CRAN"), "true")
  }
  
  out <- list()
  
  srcs <- lapply(src_names, function(x) {
    out <- NULL
    try(out <- f(x), silent = TRUE)
    if (is.null(out) && !quiet) {
      message("Could not instantiate ", x, " src")
    }
    out
  })
  
  compact(setNames(srcs, src_names))
}


db_location <- function(path = NULL, filename) {
  if (!is.null(path)) {
    # Check that path is a directory and is writeable
    if (!file.exists(path) || !file.info(path)$isdir) {
      stop(path, " is not a directory", call. = FALSE)
    }
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
