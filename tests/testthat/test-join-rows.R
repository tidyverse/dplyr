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
  expect_equal(out$x, c(2L, NA, NA))
  expect_equal(out$y, c(3L, 1L, 2L))
})

test_that("full join contains all keys from both", {
  out <- join_rows(c(2, 1), c(3, 1), type = "full")
  expect_equal(out$x, c(1L, 2L, NA))
  expect_equal(out$y, c(NA, 2L, 1L))
})

test_that("join_rows() gives meaningful error message on incompatible types", {
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = 1),
      data.frame(x = factor("a"))
    )
  )
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

test_that("join_rows() gives meaningful error message on duplicated keys", {
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 1)),
      data.frame(x = c(3, 1)),
      check_duplicates = "x"
    )
  )

  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 2)),
      data.frame(x = c(3, 1, 3)),
      check_duplicates = "y"
    )
  )

  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 1)),
      data.frame(x = 1),
      check_duplicates = "both"
    )
  )

  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = 1),
      data.frame(x = c(1, 1)),
      check_duplicates = "both"
    )
  )
})

test_that("join_rows() gives meaningful error message on missing `x` keys", {
  expect_snapshot(error = TRUE,
    join_rows(
      c(1, 2, 3, NA),
      c(1, 2, NA, 3),
      na_matches = "error"
    )
  )
})
