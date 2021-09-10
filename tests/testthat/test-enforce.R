# ------------------------------------------------------------------------------
# enforce()

test_that("pluralizes correctly", {
  df <- tibble(x = 1:2)
  expect_snapshot(error = TRUE, enforce(df, x == 1, x > 3))
})

test_that("returns `.data` invisibly", {
  df <- tibble(x = 1:2)
  expect_invisible(enforce(df, x < 3))
  expect_identical(enforce(df, x < 3), df)
})

# ------------------------------------------------------------------------------
# enforce_show()

test_that("works when there are zero issues", {
  df <- tibble(x = 1)
  expect <- tibble::add_column(vec_ptype(df), .requirement = character(), .row = integer(), .before = 1L)
  expect_identical(enforce_show(df, x == 1), expect)
})

test_that("uses user provided messages if given", {
  df <- tibble(x = 1:2)
  expect_snapshot(enforce_show(df, x == 1, "x is greater than 3" = x > 1))
})

test_that("can handle duplicate user messages", {
  df <- tibble(x = 1:2)
  expect_snapshot(enforce_show(df, "oops" = x == 1, "oops" = x > 1))
})

test_that("can handle duplicate expressions", {
  df <- tibble(x = 1:2)
  expect_snapshot(enforce_show(df, x == 1, x == 1))
})

test_that("works with pre-existing columns named `.requirement` and `.row`", {
  df1 <- tibble(.requirement = 1)
  df2 <- tibble(.row = 1)

  expect_identical(
    colnames(enforce_show(df1, .requirement > 2)),
    c(".requirement", ".row", ".requirement")
  )
  expect_identical(
    colnames(enforce_show(df2, .row > 2)),
    c(".requirement", ".row", ".row")
  )
})

# ------------------------------------------------------------------------------
# enforce_last()

test_that("can get the last enforce failure tibble", {
  df <- tibble(x = 1:2)
  expect_error(enforce(df, x == 1, x > 3))

  result <- enforce_last()
  expect <- enforce_show(df, x == 1, x > 3)

  expect_identical(result, expect)
})

test_that("fails if no failures have been recorded", {
  enforce_last_set(NULL)
  expect_snapshot(error = TRUE, enforce_last())
})
