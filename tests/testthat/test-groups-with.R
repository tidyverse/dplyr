test_that("restores original class", {
  df <- data.frame(x = 1:2)
  gf <- group_by(df, x)

  expect_s3_class(with_groups(df, x, mutate), "data.frame", exact = TRUE)
  expect_s3_class(with_groups(gf, x, mutate), "grouped_df")
})

test_that(".groups = NULL ungroups", {
  gf <- group_by(tibble(x = 1:2), x)
  out <- gf %>% with_groups(NULL, mutate, y = mean(x))
  expect_equal(out$y, c(1.5, 1.5))
})

test_that(".groups is defused with context", {
  local_fn <- identity
  expect_identical(
    with_groups(mtcars, local_fn(2), mutate, disp = disp / sd(disp)),
    with_groups(mtcars, 2, mutate, disp = disp / sd(disp))
  )
})
