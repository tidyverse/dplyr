new_source <- function(...) {
  structure(list(...), class = "source")
}

source_vars <- function(x) UseMethod("source_vars")
source_name <- function(x) UseMethod("source_name")

source_translator <- function(x) UseMethod("source_translator")

as.source <- function(x) UseMethod("as.source")
is.source <- function(x) inherit(x, "source")
