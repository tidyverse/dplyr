test_that("repeated keys generate Cartesian product", {
  out <- join_rows(c(1, 1), c(1, 1))
  expect_equal(out$x, c(1L, 1L, 2L, 2L))
  expect_equal(out$y, c(1L, 2L, 1L, 2L))
})

test_that("inner join only outputs matching keys", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "inner")
  expect_equal(out$x, 2L)
  expect_equal(out$y, 3L)
})

test_that("left join contains all keys from x", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "left")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(NA, 3L))
})

test_that("right join contains all keys from y", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "right")
  expect_equal(out$x, c(2L))
  expect_equal(out$y, c(3L))
  expect_equal(out$y_extra, c(1L, 2L))
})

test_that("full join contains all keys from both", {
  out <- join_rows(c(2, 1), c(3, 1), type = "full")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(NA, 2L))
  expect_equal(out$y_extra, 1L)
})
