context("select-helpers")
# one_of ------------------------------------------------------------------

test_that("one_of gives useful error", {
  vars <- c("x", "y")

  expect_error(one_of("z", vars = vars), "Unknown variables: `z`")
  expect_error(one_of(c("x", "z"), vars = vars), "Unknown variables: `z`")
})

