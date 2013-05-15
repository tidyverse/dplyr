#' @S3method group_by source_sqlite
group_by.source_sqlite <- function(x, ..., name = NULL) {
  name <- name %||% x$table

  vars <- named_dots(...)
  group_by <- partial_eval(x, vars, parent.frame())

  grouped_sqlite(x, vars, group_by)
}

grouped_sqlite <- function(source, vars, group_by) {
  source$vars <- vars
  source$group_by <- group_by

  structure(source, class = c("grouped_sqlite", "source_sqlite", "source"))
}

#' @S3method print grouped_sqlite
print.grouped_sqlite <- function(x, ...) {
  cat("Source: SQLite [", x$path, "]\n", sep = "")
  cat("Table:  ", x$table, dim_desc(x), "\n", sep = "")
  cat("Groups: ", paste0(deparse_all(x$vars), collapse = ", "), "\n", sep = "")

  cat("\n")
  trunc_mat(x)
}
