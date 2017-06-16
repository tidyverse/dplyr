context("Nth value")

test_that("nth works with lists", {
  x <- list(1, 2, 3)

  expect_equal(nth(x, 1), 1)
  expect_equal(nth(x, 4), NULL)
  expect_equal(nth(x, 4, default = 1), 1)
})

test_that("negative values index from end", {
  x <- 1:5

  expect_equal(nth(x, -1), 5)
  expect_equal(nth(x, -3), 3)
})

test_that("indexing past ends returns default value", {
  expect_equal(nth(1:4, 5), NA_integer_)
  expect_equal(nth(1:4, -5), NA_integer_)
  expect_equal(nth(1:4, -10), NA_integer_)
})

test_that("first uses default value for 0 length vectors", {
  expect_equal(first(logical()), NA)
  expect_equal(first(integer()), NA_integer_)
  expect_equal(first(numeric()), NA_real_)
  expect_equal(first(character()), NA_character_)
  expect_equal(first(list()), NULL)
})

test_that("firsts uses default value for 0 length augmented vectors", {
  fc <- factor("a")[0]
  dt <- Sys.Date()
  tm <- Sys.time()

  expect_equal(first(fc[0]), fc[NA])
  expect_equal(first(dt[0]), dt[NA])
  expect_equal(first(tm[0]), tm[NA])
})
