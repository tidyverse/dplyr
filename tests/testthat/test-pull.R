context("pull")

test_that("default extracts last var from data frame", {
  df <- data_frame(x = 1:10, y = 1:10)
  expect_equal(pull(df), 1:10)
})

test_that("default extracts last var from remote source", {
  mf <- memdb_frame(x = 1:10, y = 1:10)
  expect_equal(pull(mf), 1:10)
})

# find_var ----------------------------------------------------------------

test_that("errors for bad inputs", {
  expect_error(find_var(letters, letters), "of length 1")
  expect_error(find_var(quote(a), letters), "of length 1")

  expect_error(find_var("aa", letters), "Unknown variable")

  expect_error(find_var(0, letters), "must take a value")
  expect_error(find_var(100, letters), "must take a value")
  expect_error(find_var(-Inf, letters), "must take a value")
  expect_error(find_var(NA_integer_, letters), "must take a value")
})
