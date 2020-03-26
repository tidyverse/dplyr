test_that("cur_group() works", {
  df <- tibble(g = 1, x = 1)
  gf <- group_by(df, g)

  expect_equal(
    df %>% summarise(key = list(cur_group())) %>% pull(key),
    list(tibble(.rows = 1L))
  )
  expect_equal(
    gf %>% summarise(key = list(cur_group())) %>% pull(key),
    list(tibble(g = 1))
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


test_that("cur_data() gives current data without groups", {
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
})

test_that("cur_group_rows() retrieves row position in original data", {
  df <- tibble(x = c("b", "a", "b"), y = 1:3)
  gf <- group_by(df, x)

  expect_equal(
    df %>% summarise(x = list(cur_group_rows())) %>% pull(),
    list(1:3)
  )

  expect_equal(
    gf %>% summarise(x = list(cur_group_rows())) %>% pull(),
    list(2L, c(1L, 3L))
  )
})

test_that("cur_data() works sequentially", {
  df <- tibble(a = 1)
  expect_equal(
    mutate(df, x = ncol(cur_data()), y = ncol(cur_data())),
    tibble(a = 1, x = 1, y = 2)
  )
})

test_that("give useful error messages when not applicable", {
  verify_output(test_path("test-context-error.txt"), {
    n()

    cur_data()

    cur_column()
    cur_group()
    cur_group_id()
    cur_group_rows()
  })
})
