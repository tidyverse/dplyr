context("tbl_sql")

test_that("can generate sql tbls with raw sql", {
  df <- data_frame(x = 1:3, y = 3:1)
  tbls <- test_load(df, ignore = "df")

  test_f <- function(tbl) {
    clone <- tbl(x$src, build_sql("SELECT * FROM ", x$from))
    expect_equal(collect(tbl), collect(clone))
  }
})
