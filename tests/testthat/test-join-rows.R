test_that("`multiple` controls the behavior with duplicate keys", {
  # Default to `"all"`, which generates the Cartesian product
  out <- join_rows(c(1, 1), c(1, 1))
  expect_equal(out$x, c(1L, 1L, 2L, 2L))
  expect_equal(out$y, c(1L, 2L, 1L, 2L))

  out <- join_rows(c(1, 1), c(1, 1), multiple = "first")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(1L, 1L))

  out <- join_rows(c(1, 1), c(1, 1), multiple = "last")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(2L, 2L))
})

test_that("inner join only outputs matching keys", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "inner")
  expect_equal(out$x, 2L)
  expect_equal(out$y, 3L)

  out <- join_rows(c(2, 1), c(3, 4, 1), type = "inner", condition = ">")
  expect_equal(out$x, 1L)
  expect_equal(out$y, 3L)
})

test_that("left join contains all keys from x", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "left")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(NA, 3L))

  out <- join_rows(c(2, 1), c(3, 4, 1), type = "left", condition = ">")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(3L, NA))
})

test_that("right join contains all keys from y", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "right")
  expect_equal(out$x, c(2L, NA, NA))
  expect_equal(out$y, c(3L, 1L, 2L))

  out <- join_rows(c(2, 1), c(3, 4, 1), type = "right", condition = ">=")
  expect_equal(out$x, c(1L, 2L, NA, NA))
  expect_equal(out$y, c(3L, 3L, 1L, 2L))
})

test_that("full join contains all keys from both", {
  out <- join_rows(c(2, 1), c(3, 1), type = "full")
  expect_equal(out$x, c(1L, 2L, NA))
  expect_equal(out$y, c(NA, 2L, 1L))

  out <- join_rows(c(2, 1), c(3, 1), type = "full", condition = ">")
  expect_equal(out$x, c(1L, 2L, NA))
  expect_equal(out$y, c(2L, NA, 1L))
})

test_that("nest join returns 0L for unmatched x keys", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "nest")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(0L, 3L))
})

test_that("matching rows can be filtered", {
  out <- join_rows(c(3, 5), c(2, 4, 1), condition = ">=", filter = "max")
  expect_equal(out$x, 1:2)
  expect_equal(out$y, 1:2)

  out <- join_rows(c(3, 5), c(2, 4, 1), condition = ">=", filter = "min")
  expect_equal(out$x, 1:2)
  expect_equal(out$y, c(3, 3))
})

test_that("join_rows() gives meaningful error message on incompatible types", {
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = 1),
      data.frame(x = factor("a"))
    )
  )
})

test_that("join_rows() gives meaningful error/warning message on multiple matches", {
  expect_snapshot(error = TRUE, join_rows(1, c(1, 1), multiple = "error"))

  cnd <- catch_cnd(join_rows(1, c(1, 1), multiple = "warning"), classes = "warning")
  expect_snapshot(cat(cnd$message))
})

test_that("join_rows() gives meaningful error message on unmatched rows", {
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 2)),
      data.frame(x = c(3, 1)),
      check_unmatched = "x"
    )
  )

  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 2)),
      data.frame(x = c(3, 1)),
      check_unmatched = "y"
    )
  )

  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 2)),
      data.frame(x = 1),
      check_unmatched = "both"
    )
  )

  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = 1),
      data.frame(x = c(1, 2)),
      check_unmatched = "both"
    )
  )
})
