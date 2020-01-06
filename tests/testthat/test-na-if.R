context("na_if")

test_that("scalar y replaces all matching x", {
  x <- c(0, 1, 0)
  expect_equal(na_if(x, 0), c(NA, 1, NA))
  expect_equal(na_if(x, 1), c(0, NA, 0))
})


# Errors ------------------------------------------------------------------

test_that("na_if() gives meaningful errors", {
  verify_output(test_path("test-na-if.txt"), {
    na_if(1:3, 1:2)
    na_if(1, 1:2)
  })
})
