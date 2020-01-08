test_that("src_sqlite() errs if path does not exist", {
  skip_if_not_installed("dbplyr")

  expect_error(
    src_sqlite(":memory:"),
    "`path` must already exist, unless `create` = TRUE",
    fixed = TRUE
  )
})
