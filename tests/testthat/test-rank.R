# ranking functions -------------------------------------------------------

test_that("ranking empty vector returns empty vector (#762)", {
  x <- numeric()

  expect_equal(row_number(x), numeric())
  expect_equal(min_rank(x), numeric())
  expect_equal(dense_rank(x), numeric())
  expect_equal(percent_rank(x), numeric())
  expect_equal(cume_dist(x), numeric())
  expect_equal(ntile(x, 1), numeric())
})

test_that("rank functions deal pass NA (and NaN) through (#774, #1132)", {
  x <- c(1, 2, NA, 1, 0, NaN)

  expect_equal(percent_rank(x), c(1 / 3, 1, NA, 1 / 3, 0, NA))
  expect_equal(min_rank(x), c(2L, 4L, NA, 2L, 1L, NA))
  expect_equal(dense_rank(x), c(2L, 3L, NA, 2L, 1L, NA))
  expect_equal(cume_dist(x), c(.75, 1, NA, .75, .25, NA))
  expect_equal(row_number(x), c(2L, 4L, NA, 3L, 1L, NA))
})

test_that("ranking functions can handle data frames", {
  # Explicitly testing partially/fully incomplete rows
  df <- tibble(
    year = c(2020, 2020, 2021, 2020, 2020, NA),
    month = c(3, 2, 1, 2, NA, NA)
  )

  expect_identical(row_number(df), c(3L, 1L, 4L, 2L, NA, NA))
  expect_identical(min_rank(df), c(3L, 1L, 4L, 1L, NA, NA))
  expect_identical(dense_rank(df), c(2L, 1L, 3L, 1L, NA, NA))

  expect_identical(percent_rank(df), c(2 / 3, 0 / 3, 3 / 3, 0 / 3, NA, NA))
  expect_identical(cume_dist(df), c(3 / 4, 2 / 4, 4 / 4, 2 / 4, NA, NA))

  expect_identical(ntile(df, 2), c(2L, 1L, 2L, 1L, NA, NA))
  expect_identical(ntile(df, 4), c(3L, 1L, 4L, 2L, NA, NA))
})

# row_number() --------------------------------------------------------------

test_that("zero-arg row_number() works in mutate", {
  n <- c(1, 5, 2, 9)

  df <- tibble(id = rep(letters[1:4], n))
  expect_equal(mutate(df, rn = row_number())$rn, 1:sum(n))

  gf <- group_by(df, id)
  expect_equal(mutate(gf, rn = row_number())$rn, sequence(n))
})

# ntile() -------------------------------------------------------------------

test_that("ntile puts biggest groups first (#4995) ", {
  expect_equal(ntile(1, 5), 1)
  expect_equal(ntile(1:2, 5), 1:2)
  expect_equal(ntile(1:3, 5), 1:3)
  expect_equal(ntile(1:4, 5), 1:4)
  expect_equal(ntile(1:5, 5), 1:5)
  expect_equal(ntile(1:6, 5), c(1, 1:5))

  expect_equal(ntile(1, 7), 1)
  expect_equal(ntile(1:2, 7), 1:2)
  expect_equal(ntile(1:3, 7), 1:3)
  expect_equal(ntile(1:4, 7), 1:4)
  expect_equal(ntile(1:5, 7), 1:5)
  expect_equal(ntile(1:6, 7), 1:6)
  expect_equal(ntile(1:7, 7), 1:7)
  expect_equal(ntile(1:8, 7), c(1, 1:7))
})

test_that("ntile ignores NAs", {
  x <- c(1:3, NA, NA, NA)
  expect_equal(ntile(x, 3), x)

  x1 <- c(1L, 1L, 1L, NA, NA, NA)
  expect_equal(ntile(x, 1), x1)
})

test_that("ntile always returns an integer", {
  expect_equal(ntile(numeric(), 3), integer())
  expect_equal(ntile(NA, 3), NA_integer_)
})

test_that("ntile() does not overflow (#4186)", {
  out <- ntile(1:1e5, n = 1e5)
  expect_equal(out, 1:1e5)
})

test_that("ntile() works with one argument (#3418)", {
  df <- tibble(id = c(1, 1, 2, 2, 2), x = 1:5)
  expect_equal(mutate(df, out = ntile(n = 3))$out, c(1, 1, 2, 2, 3))

  gf <- group_by(df, id)
  expect_equal(mutate(gf, out = ntile(n = 2))$out, c(1, 2, 1, 1, 2))
})

test_that("ntile() validates `n`", {
  expect_snapshot(error = TRUE, {
    ntile(1, n = 1.5)
  })
  expect_snapshot(error = TRUE, {
    ntile(1, n = c(1, 2))
  })
  expect_snapshot(error = TRUE, {
    ntile(1, n = NA_real_)
  })
  expect_snapshot(error = TRUE, {
    ntile(1, n = 0)
  })
})
