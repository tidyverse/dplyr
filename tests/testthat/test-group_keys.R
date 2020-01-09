context("group_keys()")

test_that("group_keys() works", {
  tbl <- tibble(x = 1:4, g = factor(rep(c("a", "b"), each = 2), levels = c("a", "b", "c")))
  res <- tbl %>% group_by(g) %>% group_keys()
  expect_equal(res, tibble(g = factor(c("a", "b"), levels = c("a", "b", "c"))))
})

test_that("group_keys.rowwise_df() is a 0 columns data frame of the right number of rows", {
  expect_equal(
    group_keys(rowwise(iris)),
    tibble::new_tibble(list(), nrow = nrow(iris))
  )
})

