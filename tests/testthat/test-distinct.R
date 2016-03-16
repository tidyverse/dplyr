context("Distinct")

df <- data.frame(
  x = c(1, 1, 1, 1),
  y = c(1, 1, 2, 2),
  z = c(1, 1, 2, 2)
)
tbls <- test_load(df)

test_that("distinct equivalent to local unique when keep_all is TRUE", {
  compare_tbls(tbls, function(x) x %>% distinct(x, y, z, .keep_all = TRUE), ref = unique(df))
})

test_that("distinct removes duplicates (sql)", {
  skip_if_no_sqlite()
  expect_error(nrow(distinct(tbls$sqlite, x)), "specified columns")
})

test_that("distinct works for 0-sized columns (#1437)", {
  df <- data_frame(x = 1:10) %>% select(-x)
  ddf <- distinct(df, x)
  expect_equal( ncol(ddf), 0L )
})

test_that("distinct requires some variables", {
  df <- data_frame(x = 1)
  expect_error(distinct(df), "No variables selected")
})

test_that("by default distinct keeps only specified cols", {
  df <- data_frame(x = c(1, 1, 1))

  expect_equal(df %>% distinct(x), data_frame(x = 1))
  expect_equal(df %>% group_by(x) %>% distinct(), data_frame(x = 1))
})
