context("tbl_cube")

test_that("coercion", {
  grid <- expand.grid(x=letters[1:3], y=letters[1:5], stringsAsFactors = FALSE)
  tbl <- table(x=grid$x, y=grid$y)
  tbl_as_df <- as.data.frame(tbl, stringsAsFactors = FALSE)
  expect_message(cube <- as.tbl_cube(tbl_as_df), "Using Freq as")
  expect_identical(cube$dims, list(x=letters[1:3], y=letters[1:5]))
  expect_identical(names(cube$mets), "Freq")

  expect_message(cube_met <- as.tbl_cube(tbl_as_df, met_name = "Freq"), NA)
  expect_identical(cube, cube_met)

  expect_message(cube_dim <- as.tbl_cube(tbl_as_df, dim_names = c("x", "y")), NA)
  expect_identical(cube, cube_dim)

  expect_message(cube_tbl <- as.tbl_cube(tbl), NA)
  expect_identical(cube, cube_tbl)
})

test_that("summarise works with single group", {
  by_month <- group_by(nasa, month)

  out <- summarise(by_month, temp = mean(temperature))

  expect_equal(names(out$dims), "month")
  expect_equal(names(out$mets), "temp")
  expect_equal(dim(out), c(12, 1))
})
