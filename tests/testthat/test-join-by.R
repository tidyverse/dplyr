# ------------------------------------------------------------------------------
# `as_join_by()`

test_that("as_join_by() emits useful errors", {
  expect_snapshot(error = TRUE, as_join_by(FALSE))
})

# ------------------------------------------------------------------------------
# `join_by_common()`

test_that("automatically finds common variables", {
  x_names <- c("x", "y")
  y_names <- c("x", "z")
  expect_message(by <- join_by_common(x_names, y_names))
  expect_identical(by$x, "x")
  expect_identical(by$y, "x")
})

test_that("join_by_common() emits useful information", {
  # Common by message
  expect_snapshot(by <- join_by_common(c("x", "y"), c("x", "y")))

  # No common variables error
  expect_snapshot(error = TRUE, join_by_common(c("x", "y"), c("w", "z")))
})
