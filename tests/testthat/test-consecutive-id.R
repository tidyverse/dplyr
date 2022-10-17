test_that("works with simple vectors", {
  expect_equal(consecutive_id(c(1, 1, 2, 1, 2)), c(1, 1, 2, 3, 4))
})

test_that("handles data frames", {
  df <- tibble(x = c(1, 1, 1, 1), y = c(1, 2, 2, 1))
  expect_equal(consecutive_id(df), c(1, 2, 2, 3))
})

test_that("follows recycling rules", {
  expect_equal(consecutive_id(double(), 1), integer())
  expect_equal(consecutive_id(1:2, 1), 1:2)

  expect_snapshot(error = TRUE, {
    consecutive_id(1:3, 1:4)
  })
})

test_that("generates useful errors", {
  expect_snapshot(error = TRUE, {
    consecutive_id(x = 1:4)
    consecutive_id(mean)
  })
})
