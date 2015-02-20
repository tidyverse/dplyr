context("sweep_simultaneously")

test_that("simple test with min", {
  df <- data.frame(a=c(1, 2), b=c(3, 4))
  swept.df <- sweep_simultaneously(df, 2, df[1,], min)
  expect_equal(swept.df, data.frame(a=c(1, 1), b=c(3, 3)))
})