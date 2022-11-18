test_that("n_distinct() counts empty inputs", {
  expect_equal(n_distinct(NULL), 0)
  expect_equal(n_distinct(data.frame()), 0)
})

test_that("n_distinct() counts unique values in simple vectors", {
  expect_equal(n_distinct(c(TRUE, FALSE, NA)), 3)
  expect_equal(n_distinct(c(1, 2, NA)), 3)
  expect_equal(n_distinct(c(1L, 2L, NA)), 3)
  expect_equal(n_distinct(c("x", "y", NA)), 3)
})

test_that("n_distinct() counts unique combinations", {
  expect_equal(n_distinct(c(1, 1, 1), c(2, 2, 2)), 1)
  expect_equal(n_distinct(c(1, 1, 2), c(1, 2, 2)), 3)
})

test_that("n_distinct() handles data frames", {
  expect_equal(n_distinct(data.frame(c(1, 1, 1), c(2, 2, 2))), 1)
  expect_equal(n_distinct(data.frame(c(1, 1, 2), c(1, 2, 2))), 3)
})

test_that("n_distinct() can drop missing values", {
  expect_equal(n_distinct(NA, na.rm = TRUE), 0)
  expect_equal(n_distinct(c(NA, 0), na.rm = TRUE), 1)

  expect_equal(n_distinct(c(NA, 0), c(0, NA), na.rm = TRUE), 0)
  expect_equal(n_distinct(c(NA, 0), c(0, 0), na.rm = TRUE), 1)

  # check tibbles unpacked correctly
  expect_equal(n_distinct(1, tibble(x = 2, y = NA), na.rm = TRUE), 0)
})

test_that("n_distinct() follows recycling rules", {
  expect_equal(n_distinct(double(), 1), 0)
  expect_equal(n_distinct(1:2, 1), 2)
})

test_that("n_distinct() generates useful errors", {
  expect_snapshot(error = TRUE, {
    n_distinct()
    n_distinct(x = 1:4)
    n_distinct(mean)
  })
})
