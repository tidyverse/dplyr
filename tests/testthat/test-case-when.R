context("case_when")

test_that("zero inputs throws an error", {
  expect_error(case_when(), "No cases provided")
})

test_that("cases must yield compatible lengths", {
  expect_error(
    case_when(
      c(TRUE, FALSE) ~ 1,
      c(FALSE, TRUE, FALSE) ~ 2
    ),
    "LHS of case 1 is length 2"
  )

  expect_error(
    case_when(
      c(TRUE, FALSE) ~ 1:3,
      c(FALSE, TRUE) ~ 1:2
    ),
    "RHS of case 1 is length 3"
  )
})

test_that("matches values in order", {
  x <- 1:3
  expect_equal(
    case_when(
      x <= 1 ~ 1,
      x <= 2 ~ 2,
      x <= 3 ~ 3
    ),
    c(1, 2, 3)
  )
})

test_that("unmatched gets missing value", {
  x <- 1:3
  expect_equal(
    case_when(
      x <= 1 ~ 1,
      x <= 2 ~ 2
    ),
    c(1, 2, NA)
  )
})
