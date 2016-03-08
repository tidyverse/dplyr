context("select-helpers")

test_that("failed match removes all columns", {
  set_current_vars(c("x", "y"))
  on.exit(reset_current_vars())

  expect_equal(starts_with("z"), -(1:2))
  expect_equal(ends_with("z"), -(1:2))
  expect_equal(contains("z"), -(1:2))
  expect_equal(matches("z"), -(1:2))
  expect_equal(num_range("z", 1:3), -(1:2))
})

# one_of ------------------------------------------------------------------

test_that("one_of gives useful errors", {
  vars <- c("x", "y")

  expect_error(one_of("z", vars = vars), "Unknown variables: `z`")
  expect_error(one_of(c("x", "z"), vars = vars), "Unknown variables: `z`")

  expect_error(one_of(1L, vars = vars), "must be a character vector")
})

test_that("one_of converts names to positions", {
  expect_equal(one_of("a", "z", vars = letters), c(1L, 26L))
})
