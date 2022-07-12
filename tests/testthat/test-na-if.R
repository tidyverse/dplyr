test_that("scalar y replaces all matching x", {
  x <- c(0, 1, 0)
  expect_identical(na_if(x, 0), c(NA, 1, NA))
  expect_identical(na_if(x, 1), c(0, NA, 0))
})

test_that("`y` can be a vector the same length as `x` (matching SQL NULLIF)", {
  x <- c(0, 1, 0)
  y <- c(0, 1, 2)
  expect_identical(na_if(x, y), c(NA, NA, 0))
})

test_that("comparison is done with equality, so missings don't match", {
  expect_identical(na_if(NaN, NaN), NaN)
})

test_that("works when there are missings in either input", {
  expect_identical(na_if(c(1, NA, 2), 1), c(NA, NA, 2))
  expect_identical(na_if(c(1, NA, 2), c(1, NA, NA)), c(NA, NA, 2))
})

test_that("works with data frames", {
  x <- tibble(a = c(1, 99, 99, 99), b = c("x", "NA", "bar", "NA"))
  y <- tibble(a = 99, b = "NA")

  expect_identical(
    na_if(x, y),
    x[c(1, NA, 3, NA),]
  )
})

test_that("works with rcrd types", {
  x <- new_rcrd(list(a = c(1, 99, 99, 99), b = c("x", "NA", "bar", "NA")))
  y <- new_rcrd(list(a = 99, b = "NA"))

  expect_identical(
    na_if(x, y),
    x[c(1, NA, 3, NA)]
  )
})

test_that("is type stable on `x`", {
  expect_identical(na_if(0L, 0), NA_integer_)

  expect_snapshot(error = TRUE, {
    na_if(0L, 1.5)
  })
})

test_that("is size stable on `x`", {
  expect_snapshot(error = TRUE, {
    na_if(1, integer())
  })
  expect_snapshot(error = TRUE, {
    na_if(1, c(1, 2))
  })
  expect_snapshot(error = TRUE, {
    na_if(c(1, 2, 3), c(1, 2))
  })
})

test_that("requires vector types for `x` and `y`", {
  expect_snapshot(error = TRUE, {
    na_if(environment(), 1)
  })
  expect_snapshot(error = TRUE, {
    na_if(1, environment())
  })
})
