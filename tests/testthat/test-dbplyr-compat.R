context("dbplyr-compat")

test_that("compat methods work with latest dplyr", {
  expect_type(dbplyr_obj("base_agg"), "environment")
  expect_s3_class(dbplyr_fun("build_sql")("x"), "sql")
})
