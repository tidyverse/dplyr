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

test_that("cumany handles NA (#408)", {
  batman <- c(NA, NA, NA, NA, NA)
  expect_true(all(is.na(cumany(batman))))
  expect_true(all(is.na(cumall(batman))))

  x <- c(FALSE, NA)
  expect_true(all(!cumall(x)))

  x <- c(TRUE, NA)
  expect_true(all(cumany(x)))

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

  x <- 5:1; y <- 1:5
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
