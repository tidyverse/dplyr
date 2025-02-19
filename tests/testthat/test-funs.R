# between -------------------------------------------------------------------

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

test_that("takes the common type between all inputs (#6478)", {
  expect_identical(between(1L, 1.5, 2L), FALSE)
  expect_identical(between(1L, 0.5, 2.5), TRUE)

  expect_snapshot(error = TRUE, {
    between("1", 2, 3)
  })
  expect_snapshot(error = TRUE, {
    between(1, "2", 3)
  })
  expect_snapshot(error = TRUE, {
    between(1, 2, "3")
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

test_that("ptype argument works as expected with non-alphabetical ordered factors", {
  # Create an ordered factor with non-alphabetical order
  x <- factor(
    c("b", "c", "a", "d"),
    levels = c("d", "c", "b", "a"),
    ordered = TRUE
  )

  # Test with ptype specified (uses factor order)
  expect_identical(
    between(x, "c", "a", ptype = x),
    c(TRUE, TRUE, TRUE, FALSE)
  )

  # Test without ptype (uses alphabetical order)
  expect_identical(
    between(x, "c", "a"),
    c(FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("ptype argument affects type casting", {
  x <- 1:5
  expect_identical(
    between(x, 1.5, 3.5),
    c(FALSE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_snapshot(error = TRUE, {
    between(x, 1.5, 3.5, ptype = integer())
  })
})

# cum* --------------------------------------------------------------------

test_that("cum(sum,min,max) return expected results for simple cases", {
  expect_equal(cummean(numeric()), numeric())
  x <- c(5, 10, 2, 4)
  expect_equal(cummean(x), cumsum(x) / seq_along(x))

  expect_equal(cumany(logical()), logical())

  expect_equal(cumany(FALSE), FALSE)
  expect_equal(cumany(TRUE), TRUE)

  expect_equal(cumany(c(FALSE, FALSE)), c(FALSE, FALSE))
  expect_equal(cumany(c(TRUE, FALSE)), c(TRUE, TRUE))
  expect_equal(cumany(c(FALSE, TRUE)), c(FALSE, TRUE))
  expect_equal(cumany(c(TRUE, TRUE)), c(TRUE, TRUE))

  expect_equal(cumall(logical()), logical())

  expect_equal(cumall(FALSE), FALSE)
  expect_equal(cumall(TRUE), TRUE)

  expect_equal(cumall(c(FALSE, FALSE)), c(FALSE, FALSE))
  expect_equal(cumall(c(TRUE, FALSE)), c(TRUE, FALSE))
  expect_equal(cumall(c(FALSE, TRUE)), c(FALSE, FALSE))
  expect_equal(cumall(c(TRUE, TRUE)), c(TRUE, TRUE))
})

test_that("cumany/cumall propagate NAs (#408, #3749, #4132)", {
  expect_equal(cumall(c(NA, NA)), c(NA, NA))
  expect_equal(cumall(c(NA, TRUE)), c(NA, NA))
  expect_equal(cumall(c(NA, FALSE)), c(NA, FALSE))

  expect_equal(cumany(c(NA, NA)), c(NA, NA))
  expect_equal(cumany(c(NA, TRUE)), c(NA, TRUE))
  expect_equal(cumany(c(NA, FALSE)), c(NA, NA))
})

test_that("cummean is not confused by FP error (#1387)", {
  a <- rep(99, 9)
  expect_true(all(cummean(a) == a))
})
