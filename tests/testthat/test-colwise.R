context("colwise")

test_that("funs found in current environment", {
  f <- function(x) 1
  df <- data.frame(x = c(2:10, 1000))

  out <- summarise_each(df, funs(f, mean, median))
  expect_equal(out, data.frame(f = 1, mean = 105.4, median = 6.5))
})

test_that("default names are smallest unique set", {
  df <- data.frame(x = 1:3, y = 1:3)

  expect_named(summarise_each(df, funs(mean), x:y), c("x", "y"))
  expect_named(summarise_each(df, funs(mean, sd), x), c("mean", "sd"))
  expect_named(summarise_each(df, funs(mean, sd), x:y), c("x_mean", "y_mean", "x_sd", "y_sd"))
})

test_that("named arguments force complete namd", {
  df <- data.frame(x = 1:3, y = 1:3)
  expect_named(summarise_each(df, funs(mean = mean), x:y), c("x_mean", "y_mean"))
  expect_named(summarise_each(df, funs(mean, sd), x = x), c("x_mean", "x_sd"))
})

test_that("can use character vectors", {
  df <- data.frame(x = 1:3)

  expect_equal(summarise_each(df, "mean"), summarise_each(df, funs(mean)))
  expect_equal(mutate_each(df, c(mean = "mean")), mutate_each(df, funs(mean = mean)))
})
