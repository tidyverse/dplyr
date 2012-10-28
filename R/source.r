new_source <- function(...) {
  structure(list(...), class = "source")
}

source_vars <- function(x) UseMethod("source_vars")
source_name <- function(x) UseMethod("source_name")

as.source <- function(x) UseMethod("as.source")
