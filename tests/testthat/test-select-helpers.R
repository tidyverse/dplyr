context("select-helpers")
# one_of ------------------------------------------------------------------

test_that("one_of gives useful error", {
  vars <- c("x", "y")

  expect_error(select(df, one_of("z", vars = vars)), "Unknown variables: `z`")
  expect_error(select(df, one_of("x", vars = vars)), "Unknown variables: `z`")
})

