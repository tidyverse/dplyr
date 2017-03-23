context("between")

test_that("returns NA if any argument is NA", {
  expect_equal(between(1, 1, NA), NA)
  expect_equal(between(1, NA, 1), NA)
  expect_equal(between(NA, 1, 1), NA)
})

test_that("compatible with base R", {
  x <- runif(1e3)
  expect_equal(between(x, 0.25, 0.5), x >= 0.25 & x <= 0.5)
})
