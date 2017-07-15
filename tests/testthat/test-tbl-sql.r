context("tbl_sql")

test_that("can generate sql tbls with raw sql", {
  df <- data_frame(x = 1:3, y = 3:1)
  tbls <- test_load(df, ignore = "df")

  test_f <- function(tbl) {
    clone <- tbl(x$src, build_sql("SELECT * FROM ", x$from))
    expect_equal(collect(tbl), collect(clone))
  }
})

test_that("NAs in character fields handled by db sources (#2256)", {
  df <- data.frame(
    x = c("a", "aa", NA),
    y = c(NA, "b", "bb"),
    z = c("cc", NA, "c"),
    stringsAsFactors = FALSE
  )
  tbls <- test_load(df, ignore = "df")
  for (tbl in tbls) {
    expect_equal(collect(tbl), as.tbl(df))
  }
})

test_that("tbl_sql() works with string argument", {
  name <- unclass(random_table_name())
  df <- memdb_frame(a = 1, .name = name)

  expect_equal(collect(tbl_sql("sqlite", df$src, name)), collect(df))
})

test_that("memdb_frame() returns visible output", {
  expect_true(withVisible(memdb_frame(a = 1))$visible)
})
