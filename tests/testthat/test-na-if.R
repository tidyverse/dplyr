context("na_if")

test_that("error for bad y length", {
  expect_error(
    na_if(1:3, 1:2),
    "`y` must be length 3 (same as `x`) or one, not 2",
    fixed = TRUE
  )

  expect_error(
    na_if(1, 1:2),
    "`y` must be length 1 (same as `x`), not 2",
    fixed = TRUE
  )
})

test_that("scalar y replaces all matching x", {
  x <- c(0, 1, 0)
  expect_equal(na_if(x, 0), c(NA, 1, NA))
  expect_equal(na_if(x, 1), c(0, NA, 0))
})
