context("ts")

test_that("filter and lag throw errors", {
  x <- ts(1:10)

  expect_error(
    filter(x),
    "Argument `.data`: expected data source, got ts object, do you want `stats::filter()`?",
    fixed = TRUE
  )
  expect_error(
    lag(x),
    "Argument `x`: expected vector, got ts object, do you want `stats::lag()`?",
    fixed = TRUE
  )
})
