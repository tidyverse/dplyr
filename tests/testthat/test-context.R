test_that("cur_group_idx() gives unique id", {
  df <- tibble(x = c("b", "a", "b"))
  gf <- group_by(df, x)

  expect_equal(
    summarise(gf, id = cur_group_id()),
    tibble(x = c("a", "b"), id = 1:2)
  )
  expect_equal(
    mutate(gf, id = cur_group_id()),
    group_by(tibble(x = df$x, id = c(2, 1, 2)), x)
  )
})
