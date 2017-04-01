context("pull")

test_that("default extracts last var from data frame", {
  df <- data_frame(x = 1:10, y = 1:10)
  expect_equal(pull(df), 1:10)
})

# find_var ----------------------------------------------------------------

test_that("errors for bad inputs", {
  expect_error(
    find_var(letters, letters),
    "Argument `var`: must be a numeric or character scalar, got character of length 26",
    fixed = TRUE
  )
  expect_error(
    find_var(quote(a), letters),
    "Argument `var`: must be a numeric or character scalar, got symbol of length 1",
    fixed = TRUE
  )

  expect_error(
    find_var("aa", letters),
    "Column `aa`: not found",
    fixed = TRUE
  )

  expect_error(
    find_var(0, letters),
    "Argument `var`: must be a value between -26 and 26 (excluding zero), got 0",
    fixed = TRUE
  )
  expect_error(
    find_var(100, letters),
    "Argument `var`: must be a value between -26 and 26 (excluding zero), got 100",
    fixed = TRUE
  )
  expect_warning(regexp = "NAs introduced by coercion",
    expect_error(
      find_var(-Inf, letters),
      "Argument `var`: must be a value between -26 and 26 (excluding zero), got NA",
      fixed = TRUE
    )
  )
  expect_error(
    find_var(NA_integer_, letters),
    "Argument `var`: must be a value between -26 and 26 (excluding zero), got NA",
    fixed = TRUE
  )
})
