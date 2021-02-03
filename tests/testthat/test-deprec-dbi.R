test_that("src_sqlite() gives meaningful error messages", {
  skip_if_not_installed("dbplyr")

  expect_snapshot_error(src_sqlite(":memory:"))
})
