context("pull")

test_that("default extracts last var from data frame", {
  df <- tibble(x = 1:10, y = 1:10)
  expect_equal(pull(df), 1:10)
})

test_that("can extract by name, or positive/negative position", {
  x <- 1:10
  df <- tibble(x = x, y = runif(10))

  expect_equal(pull(df, x), x)
  expect_equal(pull(df, 1L), x)
  expect_equal(pull(df, 1), x)
  expect_equal(pull(df, -2), x)
  expect_equal(pull(df, -2L), x)
})

test_that("can extract named vectors", {
  x <- 1:10
  y <- letters[x]
  df <- tibble(x = x, y = y)
  xn <- set_names(x, y)

  expect_equal(pull(df, x), x)
  expect_equal(pull(df, x, y), xn)
  expect_equal(pull(df, 1, 2), xn)
  expect_equal(names(pull(df, x, y)), y)
})
