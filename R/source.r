new_source <- function(...) {
  structure(list(...), class = c("source", "ops"))
}

source_vars <- function(x) UseMethod("source_vars")
source_vars.default <- function(x) names(x)

source_name <- function(x) UseMethod("source_name")

source_translator <- function(x) UseMethod("source_translator")

print.source <- function(x, ...) {
  cat("Source: ", source_name(x), "\n")
}

as.source <- function(x) UseMethod("as.source")
is.source <- function(x) inherits(x, "source")


head.source <- function(x, ...) head(x$obj, ...)
tail.source <- function(x, ...) head(x$obj, ...)

