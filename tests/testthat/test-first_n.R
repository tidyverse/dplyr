context("test-first_n")

test_that("first_n works (#3788)", {
  expect_equal(first_n(iris, 3), slice(iris, 1:3))

  g <- group_by(iris, Species)
  expect_equal(first_n(g, 3), slice(g, 1:3))
})

test_that("first_n works with negative `n` (#3788)", {
  expect_equal(first_n(iris, -147), slice(iris, 1:3))

  g <- group_by(iris, Species)
  expect_equal(first_n(g, -49), slice(g, 1))
})

test_that("last_n works (#3788)", {
  expect_equal(last_n(iris, 3), slice(iris, 148:150))

  g <- group_by(iris, Species)
  expect_equal(last_n(g, 3), slice(g, 48:50))
})

test_that("last_n works with negative n (#3788)", {
  expect_equal(last_n(iris, -147), slice(iris, 148:150))

  g <- group_by(iris, Species)
  expect_equal(last_n(g, -49), slice(g, 50))
})
