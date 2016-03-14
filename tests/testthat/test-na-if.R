context("na_if")

test_that("error for bad y length", {
  expect_error(na_if(1:3, 1:2), "must be length 1 or same length")
})

test_that("scalar y replaces all matching x", {
  x <- c(0, 1, 0)
  expect_equal(na_if(x, 0), c(NA, 1, NA))
  expect_equal(na_if(x, 1), c(0, NA, 0))
})

test_that("is translated to NULL_IF", {
  expect_equal(translate_sql(na_if(x, 0L)), sql('NULL_IF("x", 0)'))
})
