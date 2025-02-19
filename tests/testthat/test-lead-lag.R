test_that("`lead()` / `lag()` get the direction right", {
  expect_identical(lead(1:5), c(2:5, NA))
  expect_identical(lag(1:5), c(NA, 1:4))
})

test_that("If n = 0, lead and lag return x", {
  x <- c(10L, 8L, 1L, 3L, 6L, 9L, 4L, 2L, 5L, 7L)
  expect_equal(lead(x, 0), x)
  expect_equal(lag(x, 0), x)
})

test_that("If n = length(x), returns all missing", {
  x <- c(10L, 8L, 1L, 3L, 6L, 9L, 4L, 2L, 5L, 7L)

  expect_equal(lead(x, length(x)), rep(NA_integer_, length(x)))
  expect_equal(lag(x, length(x)), rep(NA_integer_, length(x)))
})

test_that("`lag()` gives informative error for <ts> objects", {
  expect_snapshot(error = TRUE, {
    lag(ts(1:10))
  })
})

test_that("lead() and lag() work for matrices (#5028)", {
  m <- matrix(1:6, ncol = 2)
  expect_equal(
    lag(m, 1),
    matrix(c(NA_integer_, 1L, 2L, NA_integer_, 4L, 5L), ncol = 2)
  )
  expect_equal(
    lag(m, 1, default = NA),
    matrix(c(NA_integer_, 1L, 2L, NA_integer_, 4L, 5L), ncol = 2)
  )

  expect_equal(
    lead(m, 1),
    matrix(c(2L, 3L, NA_integer_, 5L, 6L, NA_integer_), ncol = 2)
  )
  expect_equal(
    lead(m, 1, default = NA),
    matrix(c(2L, 3L, NA_integer_, 5L, 6L, NA_integer_), ncol = 2)
  )
})

test_that("lead and lag preserve factors", {
  x <- factor(c("a", "b", "c"))

  expect_equal(levels(lead(x)), c("a", "b", "c"))
  expect_equal(levels(lag(x)), c("a", "b", "c"))
})

test_that("lead and lag preserves dates and times", {
  x <- as.Date("2013-01-01") + 1:3
  y <- as.POSIXct(x)

  expect_s3_class(lead(x), "Date")
  expect_s3_class(lag(x), "Date")

  expect_s3_class(lead(y), "POSIXct")
  expect_s3_class(lag(y), "POSIXct")
})

test_that("`lead()` / `lag()` validate `n`", {
  expect_snapshot(error = TRUE, {
    lead(1:5, n = 1:2)
    lead(1:5, -1)
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, n = 1:2)
    lag(1:5, -1)
  })
})

test_that("`lead()` / `lag()` check for empty dots", {
  expect_snapshot(error = TRUE, {
    lead(1:5, deault = 1)
  })
  expect_snapshot(error = TRUE, {
    lag(1:5, deault = 1)
  })
})

test_that("`lead()` / `lag()` require that `x` is a vector", {
  expect_snapshot(error = TRUE, {
    lead(environment())
  })
  expect_snapshot(error = TRUE, {
    lag(environment())
  })
})

# ------------------------------------------------------------------------------
# shift()

test_that("works with all 4 combinations of with/without `default` and lag/lead", {
  x <- 1:5

  expect_identical(shift(x, n = 2L), c(NA, NA, 1L, 2L, 3L))
  expect_identical(shift(x, n = 2L, default = 0L), c(0L, 0L, 1L, 2L, 3L))

  expect_identical(shift(x, n = -2L), c(3L, 4L, 5L, NA, NA))
  expect_identical(shift(x, n = -2L, default = 0L), c(3L, 4L, 5L, 0L, 0L))
})

test_that("works with size 0 input", {
  x <- integer()

  expect_identical(shift(x, n = 2L), x)
  expect_identical(shift(x, n = 2L, default = 3L), x)
  expect_identical(shift(x, n = -2L), x)
  expect_identical(shift(x, n = -2L, default = 3L), x)
})

test_that("works with `n = 0` with and without `default`", {
  x <- 1:5

  expect_identical(shift(x, n = 0L), x)
  expect_identical(shift(x, n = 0L, default = -1L), x)

  x <- integer()

  expect_identical(shift(x, n = 0L), x)
  expect_identical(shift(x, n = 0L, default = -1L), x)
})

test_that("works with data frames", {
  df <- tibble(a = 1:3, b = letters[1:3])

  expect_identical(shift(df, n = 1), vec_slice(df, c(NA, 1, 2)))
  expect_identical(shift(df, n = -1), vec_slice(df, c(2, 3, NA)))

  default <- tibble(a = 0L, b = "")

  expect_identical(
    shift(df, n = 2, default = default),
    vec_c(default, default, vec_slice(df, 1))
  )
})

test_that("is affected by `order_by`", {
  x <- 1:5
  order_by <- c(2, 3, 2, 1, 5)

  expect_identical(
    shift(x, n = 1, order_by = order_by),
    c(4L, 3L, 1L, NA, 2L)
  )
  expect_identical(
    shift(x, n = -2, order_by = order_by),
    c(2L, NA, 5L, 3L, NA)
  )
})

test_that("`default` is cast to the type of `x` (#6330)", {
  expect_identical(shift(1L, default = 2), 2L)

  expect_snapshot(error = TRUE, {
    shift(1L, default = 1.5)
  })
})

test_that("`default` must be size 1 (#5641)", {
  expect_snapshot(error = TRUE, {
    shift(1:5, default = 1:2)
  })
  expect_snapshot(error = TRUE, {
    shift(1:5, default = integer())
  })
})

test_that("`n` is validated", {
  expect_snapshot(error = TRUE, {
    shift(1, n = 1:2)
  })
})

test_that("`order_by` must be the same size as `x`", {
  expect_snapshot(error = TRUE, {
    shift(1:5, order_by = 1:4)
  })
})
