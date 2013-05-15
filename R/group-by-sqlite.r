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

print.grouped_sqlite <- function(x, ...) {
  cat("SQLite: ", x$path, "\n", sep = "")
  cat("Table:  ", x$table, dim_desc(x), "\n", sep = "")
  vars <- vapply(x$vars, deparse, width.cutoff = 500L, FUN.VALUE = character(1))
  cat("Groups: ", paste0(vars, collapse = ", "), "\n", sep = "")

  cat("\n")
  print(trunc_rows(x))
}
