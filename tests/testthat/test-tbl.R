context("tbl")

test_that("tbl_nongroup_vars() excludes group variables", {
  cube <- group_by(nasa, month)
  expect_identical(tbl_nongroup_vars(cube), setdiff(tbl_vars(cube), "month"))

  gdf <- group_by(mtcars, cyl)
  expect_identical(tbl_nongroup_vars(gdf), setdiff(tbl_vars(gdf), "cyl"))
})

test_that("tbl_rows_n() handles grouped data frames", {
  gdf <- group_by(mtcars, cyl)
  expect_identical(tbl_rows_n(gdf), 32L)
})
