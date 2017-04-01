context("ts")

test_that("filter and lag throw errors", {
  x <- ts(1:10)

  expect_error(
    filter(x),
    "Argument `.data`: must be a data source, got ts object, do you want `stats::filter()`?",
    fixed = TRUE
  )
  expect_error(
    lag(x),
    "Argument `x`: must be a vector, got ts object, do you want `stats::lag()`?",
    fixed = TRUE
  )
})
