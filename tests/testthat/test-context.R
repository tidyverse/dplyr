test_that("cur_group() works", {
  df <- tibble(g = 1, x = 1)
  gf <- group_by(df, g)

  expect_equal(
    df %>% summarise(cur_group()),
    group_keys(df)
  )
  expect_equal(
    gf %>% summarise(cur_group()),
    group_keys(gf)
  )

  expect_equal(
    df %>% mutate(key = cur_group()) %>% pull(key),
    group_keys(df)
  )
  expect_equal(
    gf %>% mutate(key = cur_group()) %>% pull(key),
    group_keys(gf)
  )
})

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


test_that("give useful error messages when not applicable", {
  verify_output(test_path("test-context-error.txt"), {
    n()

    cur_column()
    cur_group()
    cur_group_id()
  })
})
