context("Window functions")

test_that("If n = 0, lead and lag return x", {
  expect_equal(lead(1:2, 0), 1:2)
  expect_equal(lag(1:2, 0), 1:2)
})

test_that("If n = length(x), returns all missing", {
  miss <- rep(NA_integer_, 2)

  expect_equal(lead(1:2, 2), miss)
  expect_equal(lag(1:2, 2), miss)
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

test_that("percent_rank ignores NAs (#1132)", {
  expect_equal(percent_rank(c(1:3, NA)), c(0, 0.5, 1, NA))
})

test_that("cume_dist ignores NAs (#1132)", {
  expect_equal(cume_dist(c(1:3, NA)), c(1 / 3, 2 / 3, 1, NA))
})

test_that("cummean is not confused by FP error (#1387)", {
  a <- rep(99, 9)
  expect_true(all(cummean(a) == a))
})

test_that("order_by() returns correct value", {
  expected <- int(15, 14, 12, 9, 5)
  expect_identical(order_by(5:1, cumsum(1:5)), expected)

  x <- 5:1
  y <- 1:5
  expect_identical(order_by(x, cumsum(y)), expected)
})

test_that("order_by() works in arbitrary envs (#2297)", {
  env <- child_env("base")
  expect_equal(
    with_env(env, dplyr::order_by(5:1, cumsum(1:5))),
    rev(cumsum(rev(1:5)))
  )
  expect_equal(
    order_by(5:1, cumsum(1:5)),
    rev(cumsum(rev(1:5)))
  )
})

test_that("order_by() fails when not supplied a call (#3065)", {
  expect_error(order_by(NULL, !!1L), "`call` must be a function call, not an integer vector")
})
