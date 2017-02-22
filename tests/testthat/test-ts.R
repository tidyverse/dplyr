context("ts")

test_that("filter and lag throw errors", {
  x <- ts(1:10)

  expect_error(filter(x), "dplyr::filter")
  expect_error(lag(x), "dplyr::lag")
})
