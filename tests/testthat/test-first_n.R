context("test-first_n")

test_that("first_n works (#3788)", {
  expect_equal(first_n(iris, 3), slice(iris, 1:3))

  g <- group_by(iris, Species)
  expect_equal(first_n(g, 3), slice(g, 1:3))
})
