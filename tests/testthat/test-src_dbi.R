context("src_dbi")

test_that("multiplication works", {
  skip_if_no_sqlite()

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "mtcars", mtcars)

  mtcars2 <- tbl(con, "mtcars")
  expect_s3_class(mtcars2, "tbl_dbi")
  expect_equal(collect(mtcars2), mtcars)
})
