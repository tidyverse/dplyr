test_that("LHS can match multiple values", {
  expect_equal(case_match(1, 1:2 ~ "x"), "x")
})

test_that("LHS can match special values", {
  expect_equal(case_match(NA, NA ~ "x"), "x")
  expect_equal(case_match(NaN, NaN ~ "x"), "x")
})

test_that("RHS is recycled to match x", {
  x <- 1:3
  expect_equal(case_match(x, c(1, 3) ~ x * 2), c(2, NA, 6))
})

test_that("`NULL` values in `...` are dropped", {
  expect_identical(
    case_match(1:2, 1 ~ "a", NULL, 2 ~ "b", NULL),
    c("a", "b")
  )
})

test_that("requires at least one condition", {
  expect_snapshot(error = TRUE, {
    case_match(1)
  })
  expect_snapshot(error = TRUE, {
    case_match(1, NULL)
  })
})

test_that("passes through `.default` correctly", {
  expect_identical(case_match(1, 3 ~ 1, .default = 2), 2)
  expect_identical(case_match(1:5, 6 ~ 1, .default = 2), rep(2, 5))
  expect_identical(case_match(1:5, 6 ~ 1:5, .default = 2:6), 2:6)
})

test_that("`.default` is part of common type computation", {
  expect_identical(case_match(1, 1 ~ 1L, .default = 2), 1)

  expect_snapshot(error = TRUE, {
    case_match(1, 1 ~ 1L, .default = "x")
  })
})

test_that("passes through `.ptype` correctly", {
  expect_identical(case_match(1, 1 ~ 1, .ptype = integer()), 1L)
})

test_that("`NULL` formula element throws meaningful error", {
  expect_snapshot(error = TRUE, {
    case_match(1, 1 ~ NULL)
  })
  expect_snapshot(error = TRUE, {
    case_match(1, NULL ~ 1)
  })
})

test_that("throws chained errors when formula evaluation fails", {
  expect_snapshot(error = TRUE, {
    case_match(1, 1 ~ 2, 3 ~ stop("oh no!"))
  })
  expect_snapshot(error = TRUE, {
    case_match(1, 1 ~ 2, stop("oh no!") ~ 4)
  })
})
