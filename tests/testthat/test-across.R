# across ------------------------------------------------------------------

test_that("across() works on one column data.frame", {
  df <- data.frame(x = 1)

  out <- df %>% mutate(across())
  expect_equal(out, df)
})

test_that("across() does not select grouping variables", {
  df <- data.frame(g = 1, x = 1)

  out <- df %>% group_by(g) %>% summarise(x = across(everything())) %>% pull()
  expect_equal(out, tibble(x = 1))
})

test_that("across() correctly names output columns", {
  gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)

  expect_named(
    summarise(gf, across()),
    c("x", "y", "z", "s")
  )
  expect_named(
    summarise(gf, across(.names = "id_{col}")),
    c("x", "id_y", "id_z", "id_s")
  )
  expect_named(
    summarise(gf, across(is.numeric, mean)),
    c("x", "y", "z")
  )
  expect_named(
    summarise(gf, across(is.numeric, mean, .names = "mean_{col}")),
    c("x", "mean_y", "mean_z")
  )
  expect_named(
    summarise(gf, across(is.numeric, list(mean = mean, sum = sum))),
    c("x", "y_mean", "y_sum", "z_mean", "z_sum")
  )
  expect_named(
    summarise(gf, across(is.numeric, list(mean = mean, sum))),
    c("x", "y_mean", "y_2", "z_mean", "z_2")
  )
  expect_named(
    summarise(gf, across(is.numeric, list(mean, sum = sum))),
    c("x", "y_1", "y_sum", "z_1", "z_sum")
  )
  expect_named(
    summarise(gf, across(is.numeric, list(mean, sum))),
    c("x", "y_1", "y_2", "z_1", "z_2")
  )
  expect_named(
    summarise(gf, across(is.numeric, list(mean = mean, sum = sum), .names = "{fn}_{col}")),
    c("x", "mean_y", "sum_y", "mean_z", "sum_z")
  )
})

test_that("across() passes ... to functions", {
  df <- tibble(x = c(1, NA))
  expect_equal(
    summarise(df, across(everything(), mean, na.rm = TRUE)),
    tibble(x = 1)
  )
  expect_equal(
    summarise(df, across(everything(), list(mean = mean, median = median), na.rm = TRUE)),
    tibble(x_mean = 1, x_median = 1)
  )
})

test_that("across() passes unnamed arguments following .fns as ... (#4965)", {
  df <- tibble(x = 1)
  expect_equal(mutate(df, across(x, `+`, 1)), tibble(x = 2))
})

test_that("across() avoids simple argument name collisions with ... (#4965)", {
  df <- tibble(x = c(1, 2))
  expect_equal(summarize(df, across(x, tail, n = 1)), tibble(x = 2))
})

test_that("across() works sequentially (#4907)", {
  df <- tibble(a = 1)
  expect_equal(
    mutate(df, x = ncol(across(is.numeric)), y = ncol(across(is.numeric))),
    tibble(a = 1, x = 1L, y = 2L)
  )
  expect_equal(
    mutate(df, a = "x", y = ncol(across(is.numeric))),
    tibble(a = "x", y = 0L)
  )
  expect_equal(
    mutate(df, x = 1, y = ncol(across(is.numeric))),
    tibble(a = 1, x = 1, y = 2L)
  )
})

test_that("across() retains original ordering", {
  df <- tibble(a = 1, b = 2)
  expect_named(mutate(df, a = 2, x = across())$x, c("a", "b"))
})

test_that("across() gives meaningful messages", {
  verify_output(test_path("test-across-errors.txt"), {
    tibble(x = 1) %>%
      summarise(res = across(is.numeric, 42))
  })
})

test_that("monitoring cache - across() can be used twice in the same expression", {
  df <- tibble(a = 1, b = 2)
  expect_equal(
    mutate(df, x = ncol(across(is.numeric)) + ncol(across(a))),
    tibble(a = 1, b = 2, x = 3)
  )
})

test_that("monitoring cache - across() can be used in separate expressions", {
  df <- tibble(a = 1, b = 2)
  expect_equal(
    mutate(df, x = ncol(across(is.numeric)), y = ncol(across(a))),
    tibble(a = 1, b = 2, x = 2, y = 1)
  )
})

test_that("monitoring cache - across() usage can depend on the group id", {
  df <- tibble(g = 1:2, a = 1:2, b = 3:4)
  df <- group_by(df, g)

  switcher <- function() {
    if_else(cur_group_id() == 1L, across(a)$a, across(b)$b)
  }

  expect <- df
  expect$x <- c(1L, 4L)

  expect_equal(
    mutate(df, x = switcher()),
    expect
  )
})

# c_across ----------------------------------------------------------------

test_that("selects and combines columns", {
  df <- data.frame(x = 1:2, y = 3:4)
  out <- df %>% summarise(z = list(c_across(x:y)))
  expect_equal(out$z, list(1:4))
})
