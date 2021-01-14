test_that("_each() are defunct", {
  df <- data.frame(x = 1:3, y = 1:3)

  expect_error(summarise_each(df, list(mean)), "defunct")
  expect_error(mutate_each(df, list(mean)), "defunct")
})
