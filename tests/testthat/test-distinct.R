context("Distinct")

df <- data.frame(
  x = c(1, 1, 1, 1),
  y = c(1, 1, 2, 2),
  z = c(1, 2, 1, 2)
)
tbls <- test_load(df)

test_that("distinct equivalent to local unique when keep_all is TRUE", {
  compare_tbls(tbls, function(x) x %>% distinct(), ref = unique(df))
})

test_that("distinct for single column works as expected (#1937)", {
  compare_tbls(tbls, function(x) x %>% distinct(x, .keep_all = FALSE), ref = df[1, "x", drop= FALSE])
  compare_tbls(tbls, function(x) x %>% distinct(y, .keep_all = FALSE), ref = df[c(1, 3), "y", drop= FALSE])
})

test_that("distinct throws error if column is specified and .keep_all is TRUE", {
  skip_if_no_sqlite()
  expect_error(collect(distinct(tbls$sqlite, x, .keep_all = TRUE)),
               "specified columns.*[.]keep_all")
})

test_that("distinct works for 0-sized columns (#1437)", {
  df <- data_frame(x = 1:10) %>% select(-x)
  ddf <- distinct(df, x)
  expect_equal( ncol(ddf), 0L )
})

test_that("if no variables specified, uses all", {
  df <- data_frame(x = c(1, 1), y = c(2, 2))
  expect_equal(distinct(df), data_frame(x = 1, y = 2))
})

test_that("by default distinct keeps only specified cols", {
  df <- data_frame(x = c(1, 1, 1))

  expect_equal(df %>% distinct(x), data_frame(x = 1))
  expect_equal(df %>% group_by(x) %>% distinct(), data_frame(x = 1))
})

test_that("unless .keep_all = TRUE", {
  df <- data_frame(x = c(1, 1, 1), y = 3:1)

  expect_equal(df %>% distinct(x), data_frame(x = 1))
  expect_equal(df %>% distinct(x, .keep_all = TRUE), data_frame(x = 1, y = 3L))
})
