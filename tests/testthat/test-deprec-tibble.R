test_that("add_rownames is deprecated", {
  expect_warning(
    res <- mtcars %>% add_rownames("Make&Model"),
    "deprecated"
  )
  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
})
