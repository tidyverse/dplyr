context("Window functions")

test_that("If n = 0, lead and lag return x", {
  expect_equal(lead(1:2, 0), 1:2)
  expect_equal(lag(1:2, 0), 1:2)
})

test_that("If n = length(x), returns all missing", {
  miss <- rep(NA_integer_, 2)
  
  expect_equal(lead(1:2, 2), miss)
  expect_equal(lag(1:2, 2), miss)
  
})