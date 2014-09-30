context("colwise")

test_that("funs found in current environment", {
  f <- function(x) 1
  df <- data.frame(x = c(2:10, 1000))

  out <- summarise_each(df, funs(f, mean, median))
  expect_equal(out, data.frame(f = 1, mean = 105.4, median = 6.5))

})
