context("tbl_cube")

test_that("construction errors", {
  expect_error(
    tbl_cube(1:3, 1:3),
    "`dimensions` must be a named list of vectors, not an integer vector",
    fixed = TRUE
  )

  expect_error(
    tbl_cube(list(a = 1:3), 1:3),
    "`measures` must be a named list of arrays, not an integer vector",
    fixed = TRUE
  )

  expect_error(
    tbl_cube(list(a = 1:3), list(b = 1:3)),
    "`measures` must be a named list of arrays, not a list",
    fixed = TRUE
  )

  expect_error(
    tbl_cube(list(a = 1:3), list(b = array(1:3), c = array(1:2))),
    "Measure `c` needs dimensions [3], not [2]",
    fixed = TRUE
  )
})

test_that("coercion", {
  grid <- expand.grid(x = letters[1:3], y = letters[1:5], stringsAsFactors = FALSE)
  tbl <- table(x = grid$x, y = grid$y)
  tbl_as_df <- as.data.frame(tbl, stringsAsFactors = FALSE)
  expect_message(cube <- as.tbl_cube(tbl_as_df), "Using Freq as")
  expect_identical(cube$dims, list(x = letters[1:3], y = letters[1:5]))
  expect_identical(names(cube$mets), "Freq")

  expect_message(cube_met <- as.tbl_cube(tbl_as_df, met_name = "Freq"), NA)
  expect_identical(cube, cube_met)

  expect_message(cube_dim <- as.tbl_cube(tbl_as_df, dim_names = c("x", "y")), NA)
  expect_identical(cube, cube_dim)

  expect_message(cube_tbl <- as.tbl_cube(tbl), NA)
  expect_identical(cube, cube_tbl)
})

test_that("incomplete", {
  d <- rbind(
    cbind(data.frame(s = 1), expand.grid(j = 1)),
    cbind(data.frame(s = 2), expand.grid(j = 1:2))
  )
  d$value <- 1:3

  cube <- as.tbl_cube(d, met_name = "value")
  expect_true(is.na(as.data.frame(filter(cube, s == 1, j == 2))[["value"]]))
  expect_equal(filter(as_tibble(cube), s != 1 | j != 2), as_tibble(d))
})

test_that("duplicate", {
  d <- rbind(
    cbind(data.frame(s = 1), expand.grid(j = c(1, 1))),
    cbind(data.frame(s = 2), expand.grid(j = 1:2))
  )
  d$value <- 1:4

  expect_error(
    as.tbl_cube(d, met_name = "value"),
    "`x` must be unique in all combinations of dimension variables, duplicates: `s` = 1, `j` = 1",
    fixed = TRUE
  )
})

test_that("filter", {
  expect_equal(
    nasa %>% filter(month == 1) %>% filter(year == 2000),
    nasa %>% filter(year == 2000) %>% filter(month == 1)
  )

  expect_equal(
    nasa %>% filter(month == 1) %>% filter(year == 2000),
    filter(nasa, month == 1, year == 2000)
  )

  expect_equal(
    filter(nasa, month == 1, year == 2000),
    filter(nasa, year == 2000, month == 1)
  )

  expect_error(
    filter(nasa, month == 1 & year == 2000),
    "`month == 1 & year == 2000` must refer to exactly one dimension, not `month`, `year`"
  )
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

  expect_identical(
    tbl_df(as.data.frame(slice, stringsAsFactors = FALSE)),
    as_tibble(slice)
  )
})

test_that("can coerce to table", {
  expect_is(as.table(nasa), "table")
  expect_equal(length(dim(as.table(nasa))), 4L)
  expect_equal(dimnames(as.table(nasa)), lapply(nasa$dims, as.character))
  expect_equal(as.vector(as.table(nasa)), as.vector(nasa$mets[[1]]))
  expect_identical(as.table(nasa, measure = "ozone"), as.table(select(nasa, ozone)))
})

test_that("group_vars() returns variables", {
  gcube <- group_by(nasa, month)
  expect_identical(group_vars(gcube), "month")
})
