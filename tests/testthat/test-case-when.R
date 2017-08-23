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
      c(FALSE, TRUE, FALSE) ~ 2,
      c(FALSE, TRUE, FALSE, NA) ~ 3
    ),
    "`c(FALSE, TRUE, FALSE) ~ 2`, `c(FALSE, TRUE, FALSE, NA) ~ 3` must be length 2 or one, not 3, 4",
    fixed = TRUE
  )

  expect_error(
    case_when(
      c(TRUE, FALSE) ~ 1:3,
      c(FALSE, TRUE) ~ 1:2
    ),
    "`c(TRUE, FALSE) ~ 1:3` must be length 2 or one, not 3",
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

test_that("NA conditions (#2927)", {
  expect_equal(
    case_when(
      c(TRUE, FALSE, NA) ~ 1:3,
      TRUE ~ 4L
    ),
    c(1L, 4L, 4L)
  )
})

test_that("atomic conditions (#2909)", {
  expect_equal(
    case_when(
      TRUE ~ 1:3,
      FALSE ~ 4:6
    ),
    1:3
  )
  expect_equal(
    case_when(
      NA ~ 1:3,
      TRUE ~ 4:6
    ),
    4:6
  )
})

test_that("zero-length conditions and values (#3041)", {
  expect_equal(
    case_when(
      TRUE ~ integer(),
      FALSE ~ integer()
    ),
    integer()
  )
  expect_equal(
    case_when(
      logical() ~ 1,
      logical() ~ 2
    ),
    numeric()
  )
})
