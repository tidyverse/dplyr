context("Distinct")

test_that("distinct equivalent to local unique when keep_all is TRUE", {
  df <- tibble(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2)
  )

  expect_equal(distinct(df), unique(df))
})

test_that("distinct for single column works as expected (#1937)", {
  df <- tibble(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2)
  )

  expect_equal(distinct(df, x, .keep_all = FALSE), unique(df["x"]))
  expect_equal(distinct(df, y, .keep_all = FALSE), unique(df["y"]))
})

test_that("distinct works for 0-sized columns (#1437)", {
  df <- data_frame(x = 1:10) %>% select(-x)
  ddf <- distinct(df)
  expect_equal(ncol(ddf), 0L)
})

test_that("if no variables specified, uses all", {
  df <- data_frame(x = c(1, 1), y = c(2, 2))
  expect_equal(distinct(df), data_frame(x = 1, y = 2))
})

test_that("distinct keeps only specified cols", {
  df <- data_frame(x = c(1, 1, 1), y = c(1, 1, 1))
  expect_equal(df %>% distinct(x), data_frame(x = 1))
})

test_that("unless .keep_all = TRUE", {
  df <- data_frame(x = c(1, 1, 1), y = 3:1)

  expect_equal(df %>% distinct(x), data_frame(x = 1))
  expect_equal(df %>% distinct(x, .keep_all = TRUE), data_frame(x = 1, y = 3L))
})

test_that("distinct doesn't duplicate columns", {
  df <- tibble(a = 1:3, b = 4:6)

  expect_named(df %>% distinct(a, a), "a")
  expect_named(df %>% group_by(a) %>% distinct(a), "a")
})


test_that("grouped distinct always includes group cols", {
  df <- tibble(g = c(1, 2), x = c(1, 2))

  out <- df %>% group_by(g) %>% distinct(x)
  expect_equal(df, out)
})

test_that("empty grouped distinct equivalent to empty ungrouped", {
  df <- tibble(g = c(1, 2), x = c(1, 2))

  df1 <- df %>% distinct() %>% group_by(g)
  df2 <- df %>% group_by(g) %>% distinct()

  expect_equal(df1, df2)
})
