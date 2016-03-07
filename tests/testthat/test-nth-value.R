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

test_that("first and last use default value for 0 length inputs", {
  expect_equal(first(numeric()), NA_real_)
  expect_equal(last(numeric()), NA_real_)
})

test_that("default value returns appropriate missing for basic vectors", {
  expect_equal(default_missing(TRUE), NA)
  expect_equal(default_missing(1), NA_real_)
  expect_equal(default_missing(1L), NA_integer_)
  expect_equal(default_missing("a"), NA_character_)
  expect_equal(default_missing(list()), NULL)
})

test_that("default value errors for complicated structures", {
  expect_error(default_missing(factor("a")), "generate default for object")
  expect_error(default_missing(mtcars), "generate default for object")
})
