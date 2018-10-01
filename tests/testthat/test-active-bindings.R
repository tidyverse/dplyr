context("test-active-bindings")

test_that("Garbage collection keeps active bindings intact", {
  df <- tibble(a = 1:3, b = 3:1)

  res_df <-
    df %>%
    group_by(b) %>%
    mutate(c = { gc(); a }, d = { gc(); b }) %>%
    ungroup()

  expect_equal(df, res_df %>% select(a = c, b = d))
})
