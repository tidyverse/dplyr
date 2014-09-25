library(data.table, warn.conflicts = FALSE)
library(RSQLite, quietly = TRUE)
library(RSQLite.extfuns, quietly = TRUE)
library(RPostgreSQL, quietly = TRUE)

int_to_num <- function(x, y) {
  is_integer_x <- vapply(x, is.integer, logical(1))
  is_integer_y <- vapply(y, is.integer, logical(1))

  x[is_integer_x] <- lapply(x[is_integer_x], as.numeric)
  y[is_integer_y] <- lapply(y[is_integer_y], as.numeric)

  equal_data_frame(x, y)
}
