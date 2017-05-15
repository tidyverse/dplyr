context("pull")

test_that("default extracts last var from data frame", {
  df <- data_frame(x = 1:10, y = 1:10)
  expect_equal(pull(df), 1:10)
})

test_that("can extract by name, or positive/negative position", {
  x <- 1:10
  df <- data_frame(x = x, y = runif(10))

  expect_equal(pull(df, x), x)
  expect_equal(pull(df, 1L), x)
  expect_equal(pull(df, 1), x)
  expect_equal(pull(df, -2), x)
  expect_equal(pull(df, -2L), x)
})

# select_var --------------------------------------------------------------

test_that("errors for bad inputs", {
  expect_error(
    select_var(letters, letters),
    "`var` must evaluate to a single number",
    fixed = TRUE
  )

  expect_error(
    select_var(letters, aa),
    "object 'aa' not found",
    fixed = TRUE
  )

  expect_error(
    select_var(letters, 0),
    "`var` must be a value between -26 and 26 (excluding zero), not 0",
    fixed = TRUE
  )
  expect_error(
    select_var(letters, 100),
    "`var` must be a value between -26 and 26 (excluding zero), not 100",
    fixed = TRUE
  )
  expect_error(
    select_var(letters, -Inf),
    "`var` must be a value between -26 and 26 (excluding zero), not NA",
    fixed = TRUE
  )
  expect_error(
    select_var(letters, NA_integer_),
    "`var` must be a value between -26 and 26 (excluding zero), not NA",
    fixed = TRUE
  )
})
