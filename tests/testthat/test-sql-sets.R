context("sql-sets")

test_that("column order is matched", {
  skip_if_no_sqlite()

  df1 <- memdb_frame(x = 1, y = 2)
  df2 <- memdb_frame(y = 1, x = 2)

  out <- collect(union(df1, df2))
  expect_equal(out, tibble(x = c(1, 2), y = c(2, 1)))
})

test_that("missing columns filled with NULL", {
  skip_if_no_sqlite()

  df1 <- memdb_frame(x = 1)
  df2 <- memdb_frame(y = 1)

  out <- collect(union(df1, df2))
  expect_equal(out, tibble(x = c(1, NA), y = c(NA, 1)))
})
