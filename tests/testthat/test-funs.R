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


# cum* --------------------------------------------------------------------



test_that("cum(sum,min,max) works", {
  df <- data.frame(x = 1:10, y = seq(1, 10, by = 1), g = rep(c(1, 2), each = 5))

  res <- mutate(df,
    csumx = cumsum(x), csumy = cumsum(y),
    cminx = cummin(x), cminy = cummin(y),
    cmaxx = cummax(x), cmaxy = cummax(y)
  )
  expect_equal(res$csumx, cumsum(df$x))
  expect_equal(res$csumy, cumsum(df$y))
  expect_equal(res$cminx, cummin(df$x))
  expect_equal(res$cminy, cummin(df$y))
  expect_equal(res$cmaxx, cummax(df$x))
  expect_equal(res$cmaxy, cummax(df$y))

  res <- mutate(group_by(df, g),
    csumx = cumsum(x), csumy = cumsum(y),
    cminx = cummin(x), cminy = cummin(y),
    cmaxx = cummax(x), cmaxy = cummax(y)
  )
  expect_equal(res$csumx, c(cumsum(df$x[1:5]), cumsum(df$x[6:10])))
  expect_equal(res$csumy, c(cumsum(df$y[1:5]), cumsum(df$y[6:10])))
  expect_equal(res$cminx, c(cummin(df$x[1:5]), cummin(df$x[6:10])))
  expect_equal(res$cminy, c(cummin(df$y[1:5]), cummin(df$y[6:10])))
  expect_equal(res$cmaxx, c(cummax(df$x[1:5]), cummax(df$x[6:10])))
  expect_equal(res$cmaxy, c(cummax(df$y[1:5]), cummax(df$y[6:10])))

  df$x[3] <- NA
  df$y[4] <- NA
  res <- mutate(df,
    csumx = cumsum(x), csumy = cumsum(y),
    cminx = cummin(x), cminy = cummin(y),
    cmaxx = cummax(x), cmaxy = cummax(y)
  )
  expect_true(all(is.na(res$csumx[3:10])))
  expect_true(all(is.na(res$csumy[4:10])))

  expect_true(all(is.na(res$cminx[3:10])))
  expect_true(all(is.na(res$cminy[4:10])))

  expect_true(all(is.na(res$cmaxx[3:10])))
  expect_true(all(is.na(res$cmaxy[4:10])))
})


test_that("cumany and cumall handle NAs consistently (#408, #3749, #4132)", {
  batman <- c(NA, NA, NA, NA, NA)
  expect_true(all(is.na(cumany(batman))))
  expect_true(all(is.na(cumall(batman))))

  # normal usecases
  expect_identical(
    cumall(c(TRUE, NA, FALSE, NA)),
    c(TRUE, NA, FALSE, FALSE)
  )

  expect_identical(
    cumall(c(FALSE, NA, TRUE)),
    c(FALSE, FALSE, FALSE)
  )

  expect_identical(
    cumall(c(NA, TRUE)),
    c(NA, NA)
  )

  expect_identical(
    cumall(c(NA, FALSE)),
    c(NA, FALSE)
  )

  expect_identical(
    cumany(c(TRUE, NA, FALSE)),
    c(TRUE, TRUE, TRUE)
  )

  expect_identical(
    cumany(c(FALSE, NA, TRUE)),
    c(FALSE, NA, TRUE)
  )

  # scalars
  expect_true(is.na(cumall(NA)))
  expect_true(is.na(cumany(NA)))
  expect_true(cumall(TRUE))
  expect_false(cumall(FALSE))
  expect_true(cumany(TRUE))
  expect_false(cumany(FALSE))

  # degenerate cases
  expect_identical(
    cumall(logical()),
    logical()
  )

  expect_identical(
    cumany(logical()),
    logical()
  )

  # behaviour of degenerate logical vectors mimics that of base R functions
  x <- as.raw(c(2L, 9L, 0L))
  class(x) <- "logical"
  expect_identical(cumall(x), x == TRUE)
  expect_identical(cumany(x), c(TRUE, TRUE, TRUE))
})



test_that("cummean is not confused by FP error (#1387)", {
  a <- rep(99, 9)
  expect_true(all(cummean(a) == a))
})

test_that("cummean is consistent with cumsum() and seq_along() (#5287)", {
  x <- 1:5
  expect_equal(cummean(x), c(1, 1.5, 2, 2.5, 3))
  expect_equal(cummean(x), cumsum(x) / seq_along(x))

  expect_equal(cummean(numeric()), numeric())
})
