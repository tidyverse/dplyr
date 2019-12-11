context("union_all")

test_that("union all on vectors concatenates", {
  expect_equal(union_all(1:3, 4:6), 1:6)
})

test_that("union all on data frames calls bind rows", {
  df1 <- tibble(x = 1:2)
  df2 <- tibble(y = 1:2)

  expect_equal(union_all(df1, df2), bind_rows(df1, df2))
})
