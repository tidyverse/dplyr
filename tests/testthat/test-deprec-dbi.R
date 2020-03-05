test_that("src_sqlite() gives meaningful error messages", {
  skip_if_not_installed("dbplyr")

  verify_output(test_path("test-deprec-dbi-errors.txt"), {
    src_sqlite(":memory:")
  })
})
