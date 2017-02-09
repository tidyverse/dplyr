context("data_frame")

# add_rownames -----------------------------------------------------------

test_that("add_rownames keeps the tbl classes (#882)", {
  expect_warning(
    res <- mtcars %>% add_rownames("Make&Model"),
    "Deprecated"
  )
  expect_equal(class(res), c("tbl_df", "tbl", "data.frame"))
})
