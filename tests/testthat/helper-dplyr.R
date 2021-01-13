expect_no_error <- function(object, ...) {
  expect_error({{ object }}, NA, ...)
}
expect_no_warning <- function(object, ...) {
  expect_warning({{ object }}, NA, ...)
}
