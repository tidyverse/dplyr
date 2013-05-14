data_table_source <- function(obj, name = deparse(substitute(obj))) {
  stopifnot(require("data.table"))
  # Hack until new version of data.table comes out
  assignInNamespace("cedta", function(...) TRUE, "data.table")

  obj <- as.data.table(obj)
  structure(list(obj = obj, name = name),
    class = c("source_data_table", "source"))
}

source_vars.source_data_table <- function(x) names(x$obj)
source_name.source_data_table <- function(x) x$name

dim.source_data_table <- function(x) dim(x$obj)
