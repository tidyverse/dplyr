test_that("src_sqlite() gives meaningful error messages", {
  skip_if_not_installed("dbplyr")

  verify_output(test_path("test-src_dbi-errors.txt"), {
    src_sqlite(":memory:")
  })
})
