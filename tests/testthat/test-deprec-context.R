test_that("cur_data() is soft deprecated", {
  options(lifecycle_verbosity = "warning")

  df <- tibble(x = 1)

  expect_snapshot(mutate(df, y = cur_data()))
})

test_that("cur_data_all() is soft deprecated", {
  options(lifecycle_verbosity = "warning")

  df <- tibble(x = 1)

  expect_snapshot(mutate(df, y = cur_data_all()))
})

test_that("cur_data() gives current data without groups, cur_data_all() includes groups", {
  options(lifecycle_verbosity = "quiet")

  df <- tibble(x = c("b", "a", "b"), y = 1:3)
  gf <- group_by(df, x)

  expect_equal(
    df %>% summarise(x = list(cur_data())) %>% pull(),
    list(df)
  )

  expect_equal(
    gf %>% summarise(x = list(cur_data())) %>% pull(),
    list(tibble(y = 2L), tibble(y = c(1L, 3L)))
  )
  expect_equal(
    gf %>% summarise(x = list(cur_data_all())) %>% pull(),
    list(tibble(x = "a", y = 2L), tibble(x = "b", y = c(1L, 3L)))
  )
})

test_that("cur_data()/cur_data_all() keeps list columns as lists in rowwise_df (#5901)", {
  options(lifecycle_verbosity = "quiet")

  df <- tibble(x = list(tibble(a = 1), tibble(a = 2))) %>%
    rowwise()

  expect_true(
    all(summarise(df, test = obj_is_list(cur_data()$x))$test)
  )
  expect_true(
    all(summarise(df, test = obj_is_list(cur_data_all()$x))$test)
  )
})

test_that("cur_data() and cur_data_all() work sequentially", {
  options(lifecycle_verbosity = "quiet")

  df <- tibble(a = 1)
  expect_equal(
    mutate(df, x = ncol(cur_data()), y = ncol(cur_data())),
    tibble(a = 1, x = 1, y = 2)
  )

  gf <- tibble(a = 1, b = 2) %>% group_by(a)
  expect_equal(
    mutate(gf, x = ncol(cur_data_all()), y = ncol(cur_data_all())),
    group_by(tibble(a = 1, b = 2, x = 2, y = 3), a)
  )
})

test_that("mutate(=NULL) preserves correct all_vars", {
  options(lifecycle_verbosity = "quiet")

  df <- data.frame(x = 1, y = 2) %>% mutate(x = NULL, vars = cur_data_all()) %>% pull()
  expect_equal(df, tibble(y = 2))
})

test_that("give useful error messages when not applicable", {
  options(lifecycle_verbosity = "quiet")

  expect_snapshot({
    (expect_error(cur_data()))
    (expect_error(cur_data_all()))
  })
})
