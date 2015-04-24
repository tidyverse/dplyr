context("tbl_sql")

srcs <- temp_srcs("sqlite", "postgres")

df <- data.frame(x = 1:3, y = 3:1)
tbls <- temp_load(srcs, list(df = df))
name <- tbls[[1]]$df$from

test_that("can generate sql tbls with raw sql", {

  raw <- lapply(srcs, function(x) tbl(x, build_sql("SELECT * FROM ", name)))
  expect_true(compare_tbls(raw, identity, ref = df))
})

test_that("complex filter condition for sqlite", {
  tbl_sqlite <- tbls$sqlite$df
  query <- tbl_sqlite %>%
    filter(x <= 2) %>%
    filter(x <= 1 || x >= 3)
  df <- query %>% collect
  expect_equal(nrow(df), 1)
  expect_equal(df$x, 1)
  expect_equal(df$y, 3)
})

test_that("complex filter condition for postgres", {
  if(!is.null(tbls$postgres)) {
    tbl_postgres <- tbls$postgres$df
    query <- tbl_postgres %>%
      filter(x <= 2) %>%
      filter(x <= 1 || x >= 3)
    df <- query %>% collect
    expect_equal(nrow(df), 1)
    expect_equal(df$x, 1)
    expect_equal(df$y, 3)
  }
})
