data_frame_source <- function(obj, name = deparse(substitute(obj))) {
  structure(list(obj = obj, name = name),
    class = c("source_data_frame", "source"))
}

source_vars.source_data_frame <- function(x) names(x$obj)
source_name.source_data_frame <- function(x) x$name
