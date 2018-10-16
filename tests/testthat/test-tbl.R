context("tbl")

test_that("tbl_nongroup_vars() excludes group variables", {
  cube <- group_by(nasa, month)
  expect_identical(tbl_nongroup_vars(cube), setdiff(tbl_vars(cube), "month"))

  gdf <- group_by(mtcars, cyl)
  expect_identical(tbl_nongroup_vars(gdf), setdiff(tbl_vars(gdf), "cyl"))
})

test_that("sel_vars() records groups", {
  gdf <- group_by(mtcars, cyl, am)
  expect_is(sel_vars(gdf), "dplyr_sel_vars")
  expect_true(is_sel_vars(sel_vars(gdf)))
  expect_identical(unstructure(sel_vars(gdf)), tbl_vars(gdf))
  expect_identical(sel_vars(gdf) %@% groups, c("cyl", "am"))
})
