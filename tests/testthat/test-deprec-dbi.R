test_that("src_sqlite() gives meaningful error messages", {
  skip_if_not_installed("dbplyr")
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_snapshot_error(src_sqlite(":memory:"))
})
