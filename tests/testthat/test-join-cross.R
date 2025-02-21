test_that("cross join works", {
  df1 <- tibble(x = 1:2)
  df2 <- tibble(y = 1:3)

  expect_identical(
    cross_join(df1, df2),
    tibble(
      x = vec_rep_each(1:2, times = 3),
      y = vec_rep(1:3, times = 2)
    )
  )
})

test_that("cross join results in 0 rows if either input has 0 rows", {
  df1 <- tibble(x = 1:2)
  df2 <- tibble(y = integer())

  expect_identical(
    cross_join(df1, df2),
    tibble(x = integer(), y = integer())
  )
  expect_identical(
    cross_join(df2, df1),
    tibble(y = integer(), x = integer())
  )
})

test_that("cross join works with 0 column, >0 row tibbles", {
  df1 <- new_tibble(list(), nrow = 3)
  df2 <- tibble(x = 1:2)

  expect_identical(
    cross_join(df1, df1),
    new_tibble(list(), nrow = 9)
  )
  expect_identical(
    cross_join(df1, df2),
    vec_rep(df2, times = 3)
  )
})

test_that("cross join applies `suffix`", {
  df1 <- tibble(x = 1, y = 2)
  df2 <- tibble(x = 2, z = 3)

  expect_named(cross_join(df1, df2), c("x.x", "y", "x.y", "z"))
  expect_named(
    cross_join(df1, df2, suffix = c("", "_y")),
    c("x", "y", "x_y", "z")
  )
})

test_that("cross join checks for duplicate names", {
  df1 <- tibble(a = 1, b = 2, a = 3, .name_repair = "minimal")
  df2 <- tibble(a = 2, c = 3)

  expect_snapshot(error = TRUE, {
    cross_join(df1, df2)
  })
})
