context("pull")

test_that("default extracts last var from data frame", {
  df <- data_frame(x = 1:10, y = 1:10)
  expect_equal(pull(df), 1:10)
})

# find_var ----------------------------------------------------------------

test_that("errors for bad inputs", {
  expect_error(
    find_var(letters, letters),
    "`var`: must be a numeric or character scalar, not character of length 26",
    fixed = TRUE
  )
  expect_error(
    find_var(quote(a), letters),
    "`var`: must be a numeric or character scalar, not symbol of length 1",
    fixed = TRUE
  )

  expect_error(
    find_var("aa", letters),
    "Column `aa`: not found",
    fixed = TRUE
  )

  expect_error(
    find_var(0, letters),
    "`var`: must be a value between -26 and 26 (excluding zero), not 0",
    fixed = TRUE
  )
  expect_error(
    find_var(100, letters),
    "`var`: must be a value between -26 and 26 (excluding zero), not 100",
    fixed = TRUE
  )
  expect_warning(regexp = "NAs introduced by coercion",
    expect_error(
      find_var(-Inf, letters),
      "`var`: must be a value between -26 and 26 (excluding zero), not NA",
      fixed = TRUE
    )
  )
  expect_error(
    find_var(NA_integer_, letters),
    "`var`: must be a value between -26 and 26 (excluding zero), not NA",
    fixed = TRUE
  )
})
