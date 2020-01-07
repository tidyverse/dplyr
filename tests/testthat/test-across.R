test_that("across() does not select grouping variables", {
  df <- data.frame(g = 1, x = 1)

  out <- df %>% group_by(g) %>% summarise(x = across(everything())) %>% pull()
  expect_equal(out, tibble(x = 1))
})
