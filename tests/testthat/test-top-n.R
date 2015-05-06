context("top_n")

test_that("top_n returns n rows", {
  test_df <- data.frame(x = 1:10, y = 11:20)
  top_four <- test_df %>% top_n(4, y)
  expect_equal(dim(top_four), c(4, 2))
})
