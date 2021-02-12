test_that("scalar y replaces all matching x", {
  x <- c(0, 1, 0)
  expect_equal(na_if(x, 0), c(NA, 1, NA))
  expect_equal(na_if(x, 1), c(0, NA, 0))
})


# Errors ------------------------------------------------------------------

test_that("na_if() gives meaningful errors", {
  expect_snapshot(error = TRUE, na_if(1:3, 1:2))
  expect_snapshot(error = TRUE, na_if(1, 1:2))
})
