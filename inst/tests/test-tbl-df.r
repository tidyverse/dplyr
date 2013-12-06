context("tbl_df/grouped_df")

test_that("grouped_df returns tbl_df if no groups", {
  df <- grouped_df(mtcars, list())
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))
})