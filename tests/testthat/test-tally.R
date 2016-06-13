context("tally")

test_that("weighted tally drops NAs (#1145)", {
  df <- data_frame(x = c(1, 1, NA))

  expect_equal(tally(df, x)$n, 2)
})
