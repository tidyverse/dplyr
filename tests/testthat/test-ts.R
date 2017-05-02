context("ts")

test_that("filter and lag throw errors", {
  x <- ts(1:10)

  expect_error(
    filter(x),
    "`.data` must be a data source, not a ts object, do you want `stats::filter()`?",
    fixed = TRUE
  )
  expect_error(
    lag(x),
    "`x` must be a vector, not a ts object, do you want `stats::lag()`?",
    fixed = TRUE
  )
})
