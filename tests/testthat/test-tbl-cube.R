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

test_that("incomplete", {
  d <- rbind(cbind(data_frame(s=1), expand.grid(j=1)),
             cbind(data_frame(s=2), expand.grid(j=1:2)))
  d$value <- 1:3
  d <- as_data_frame(d)

  cube <- as.tbl_cube(d, met_name = "value")
  expect_true(is.na(as.data.frame(filter(cube, s == 1, j == 2))[["value"]]))
  expect_equal(filter(as_data_frame(as.data.frame(cube)), s != 1 | j != 2), d)
})

test_that("duplicate", {
  d <- rbind(cbind(data_frame(s=1), expand.grid(j=c(1, 1))),
             cbind(data_frame(s=2), expand.grid(j=1:2)))
  d$value <- 1:4

  expect_error(as.tbl_cube(d, met_name = "value"), "Duplicate.*s = 1, j = 1")
})

test_that("summarise works with single group", {
  by_month <- group_by(nasa, month)

  out <- summarise(by_month, temp = mean(temperature))

  expect_equal(names(out$dims), "month")
  expect_equal(names(out$mets), "temp")
  expect_equal(dim(out), c(12, 1))
})

test_that("can coerce to data_frame", {
  slice <- filter(nasa, year == 1995L, month == 1L)

  expect_identical(tbl_df(as.data.frame(slice, stringsAsFactors = FALSE)),
                   as_data_frame(slice))
})

test_that("can coerce to table", {
  expect_is(as.table(nasa), "table")
  expect_equal(length(dim(as.table(nasa))), 4L)
  expect_equal(dimnames(as.table(nasa)), lapply(nasa$dims, as.character))
  expect_equal(as.vector(as.table(nasa)), as.vector(nasa$mets[[1]]))
  expect_identical(as.table(nasa, measure = "ozone"), as.table(select(nasa, ozone)))
})
