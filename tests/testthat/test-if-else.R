context("if_else")

test_that("first argument must be logical", {
  expect_error(if_else(1:10, 1, 2), "must be logical")
})

test_that("true and false must be same length as condition (or length 1)", {
  expect_error(if_else(1:3 < 2, 1:2, 1:3), "`true` is length 2 not 1 or 3")
  expect_error(if_else(1:3 < 2, 1:3, 1:2), "`false` is length 2 not 1 or 3")
})

test_that("true and false must be same type and same class", {
  expect_error(if_else(1:3 < 2, 1, 1L), "`false` has type 'integer'")

  x <- factor("x")
  y <- ordered("x")
  expect_error(if_else(1:3 < 2, x, y), "`false` has class ordered/factor")
})

test_that("scalar true and false are vectorised", {
  x <- c(TRUE, TRUE, FALSE, FALSE)
  expect_equal(if_else(x, 1, 2), c(1, 1, 2, 2))
})

test_that("vector true and false are ok", {
  x <- c(-1, 0, 1)

  expect_equal(if_else(x < 0, x, 0), c(-1, 0, 0))
  expect_equal(if_else(x > 0, x, 0), c(0, 0, 1))
})

test_that("missing values are missing", {
  expect_equal(if_else(c(TRUE, NA, FALSE), -1, 1), c(-1, NA, 1))
})

test_that("works with lists", {
  x <- list(1, 2, 3)

  expect_equal(
    if_else(c(TRUE, TRUE, FALSE), x, list(NULL)),
    list(1, 2, NULL)
  )
})

test_that("all forms of if translated to case statement", {
  expected <- sql('CASE WHEN ("x") THEN (1) ELSE (2) END')

  expect_equal(translate_sql(if (x) 1L else 2L), expected)
  expect_equal(translate_sql(ifelse(x, 1L, 2L)), expected)
  expect_equal(translate_sql(if_else(x, 1L, 2L)), expected)
})
