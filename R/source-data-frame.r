data_frame_source <- function(data, name = deparse(substitute(obj))) {
  structure(list(obj = data, name = name),
    class = c("source_data_frame", "source"))
}


print.source <- function(x, ...) {
  cat("Source: ", x$name, dim_desc(x), "\n", sep = "")
  cat("\n")
  print(trunc_rows(x$obj))
}


source_vars.source_data_frame <- function(x) names(x$obj)
source_name.source_data_frame <- function(x) x$name

dim.source_data_frame <- function(x) dim(x$obj)
