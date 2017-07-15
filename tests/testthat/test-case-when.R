context("case_when")

test_that("zero inputs throws an error", {
  expect_error(
    case_when(),
    "No cases provided",
    fixed = TRUE
  )
})

test_that("error messages", {
  expect_error(
    case_when(
      paste(50)
    ),
    "Case 1 (`paste(50)`) must be a two-sided formula, not a string",
    fixed = TRUE
  )

  expect_error(
    case_when(
      50 ~ 1:3
    ),
    "LHS of case 1 (`50`) must be a logical, not double",
    fixed = TRUE
  )
})

test_that("cases must yield compatible lengths", {
  expect_error(
    case_when(
      c(TRUE, FALSE) ~ 1,
      c(FALSE, TRUE, FALSE) ~ 2
    ),
    "LHS of case 1 (`c(TRUE, FALSE)`) must be length 3 (the longest input) or one, not 2",
    fixed = TRUE
  )

  expect_error(
    case_when(
      c(TRUE, FALSE) ~ 1:3,
      c(FALSE, TRUE) ~ 1:2
    ),
    "RHS of case 1 (1:3) must be length 2 (the first output) or one, not 3",
    fixed = TRUE
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

test_that("missing values can be replaced (#1999)", {
  x <- c(1:3, NA)
  expect_equal(
    case_when(
      x <= 1 ~ 1,
      x <= 2 ~ 2,
      is.na(x) ~ 0
    ),
    c(1, 2, NA, 0)
  )
})
