test_that("returns NA if any argument is NA", {
  na <- NA_real_
  expect_equal(between(1, 1, na), NA)
  expect_equal(between(1, na, 1), NA)
  expect_equal(between(na, 1, 1), NA)
})

test_that("can be vectorized along `left` and `right`", {
  expect_identical(between(1:2, c(0L, 4L), 5L), c(TRUE, FALSE))
  expect_identical(between(1:2, 0L, c(0L, 3L)), c(FALSE, TRUE))
})

test_that("compatible with base R", {
  x <- runif(1e3)
  expect_equal(between(x, 0.25, 0.5), x >= 0.25 & x <= 0.5)
})

test_that("works with S3 objects", {
  x <- new_vctr(c(1, 5), class = "foo")
  left <- new_vctr(0, class = "foo")
  right <- new_vctr(3, class = "foo")

  expect_identical(between(x, left, right), c(TRUE, FALSE))
})

test_that("works with date-time `x` and date `left/right` (#6183)", {
  jan2 <- as.POSIXct("2022-01-02", tz = "UTC")

  jan1 <- as.Date("2022-01-01")
  jan3 <- as.Date("2022-01-03")

  expect_true(between(jan2, jan1, jan3))
})

test_that("works with data frames", {
  x <- tibble(year = c(2020, 2020, 2021), month = c(1, 3, 6))
  left <- tibble(year = c(2019, 2020, 2021), month = c(1, 4, 3))
  right <- tibble(year = c(2020, 2020, 2022), month = c(1, 6, 3))

  expect_identical(between(x, left, right), c(TRUE, FALSE, TRUE))
})

test_that("works with rcrds", {
  x <- new_rcrd(list(year = c(2020, 2020, 2021), month = c(1, 3, 6)))
  left <- new_rcrd(list(year = c(2019, 2020, 2021), month = c(1, 4, 3)))
  right <- new_rcrd(list(year = c(2020, 2020, 2022), month = c(1, 6, 3)))

  expect_identical(between(x, left, right), c(TRUE, FALSE, TRUE))
})

test_that("casts `left` and `right` to the type of `x`", {
  expect_snapshot(error = TRUE, {
    between(1L, 1.5, 2L)
  })
  expect_snapshot(error = TRUE, {
    between(1L, 1L, 2.5)
  })
})

test_that("recycles `left` and `right` to the size of `x`", {
  expect_snapshot(error = TRUE, {
    between(1:3, 1:2, 1L)
  })
  expect_snapshot(error = TRUE, {
    between(1:3, 1L, 1:2)
  })
})
