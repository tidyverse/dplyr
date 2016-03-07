context("select-helpers")
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
