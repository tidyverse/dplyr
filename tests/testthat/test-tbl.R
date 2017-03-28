context("tbl")

test_that("tbl_vars() excludes group variables", {
  cube <- group_by(nasa, month)
  expect_identical(tbl_vars(cube, FALSE), setdiff(tbl_vars(cube), "month"))

  gdf <- group_by(mtcars, cyl)
  expect_identical(tbl_vars(gdf, FALSE), setdiff(tbl_vars(gdf), "cyl"))

  lazy <- mtcars %>% tbl_lazy() %>% group_by(cyl)
  expect_identical(tbl_vars(lazy, FALSE), setdiff(tbl_vars(lazy), "cyl"))
})
