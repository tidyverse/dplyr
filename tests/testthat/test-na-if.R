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

test_that("`NA` replacing itself is a no-op", {
  expect_identical(na_if(NA, NA), NA)
})

test_that("missing values are allowed to equal each other, so `NaN`s can be standardized", {
  expect_identical(na_if(NaN, NaN), NA_real_)
})

test_that("missing values equal each other in partially incomplete data frame rows", {
  x <- tibble(
    x = c(2, 1, NA, 1),
    y = c(1, NA, NA, NA),
    z = c(3, NaN, NA, NaN)
  )

  y <- tibble(x = 1, y = NA, z = NaN)

  expect <- vec_assign(x, i = c(2, 4), value = NA)

  expect_identical(na_if(x, y), expect)
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
