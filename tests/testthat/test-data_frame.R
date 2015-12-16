context("data_frame")

test_that("data_frame returns correct number of rows with all combinatinos", {

  expect_equal(nrow(data_frame(value = 1:10)), 10L)

  expect_equal(nrow(data_frame(value = 1:10, name = "recycle_me")), 10L)

  expect_equal(nrow(data_frame(name = "recycle_me", value = 1:10)), 10L)

  expect_equal(nrow(data_frame(name = "recycle_me", value = 1:10, value2 = 11:20)), 10L)

  expect_equal(nrow(data_frame(value = 1:10, name = "recycle_me", value2 = 11:20)), 10L)

})

test_that("can't make data_frame containing data.frame or array", {
  expect_error(data_frame(mtcars), "must be a 1d atomic vector or list")
  expect_error(data_frame(diag(5)), "must be a 1d atomic vector or list")
})

test_that("null isn't a valid column", {
  expect_error(data_frame(a = NULL))
  expect_error(as_data_frame(list(a = NULL)), "must be a 1d atomic vector or list")
})

test_that("length 1 vectors are recycled", {
  expect_equal(nrow(data_frame(x = 1:10)), 10)
  expect_equal(nrow(data_frame(x = 1:10, y = 1)), 10)
  expect_error(
    nrow(data_frame(x = 1:10, y = 1:2)),
    "Variables must be length 1 or 10"
  )
})

test_that("missing names are imputed from call", {
  x <- 1:10
  df <- data_frame(x, y = x)
  expect_equal(tbl_vars(df), c("x", "y"))
})

test_that("empty input makes 0 x 0 tbl_df", {
  zero <- data_frame()
  expect_is(zero, "tbl_df")
  expect_equal(dim(zero), c(0L, 0L))
})

test_that("SE version", {
  expect_identical(data_frame_(list(a = ~1:10)), data_frame(a = 1:10))
})

# as_data_frame -----------------------------------------------------------

test_that("columns must be same length", {
  l <- list(x = 1:2, y = 1:3)
  expect_error(as_data_frame(l), "must be length 1 or")
})

test_that("columns must be named", {
  l1 <- list(1:10)
  l2 <- list(x = 1, 2)

  expect_error(as_data_frame(l1), "must be named")
  expect_error(as_data_frame(l2), "must be named")
})

test_that("can't coerce list data.frame or array", {
  expect_error(as_data_frame(list(x = mtcars)), "must be a 1d atomic vector or list")
  expect_error(as_data_frame(list(x = diag(5))), "must be a 1d atomic vector or list")
})

test_that("Zero column list makes 0 x 0 tbl_df", {
  zero <- as_data_frame(list())
  expect_is(zero, "tbl_df")
  expect_equal(dim(zero), c(0L, 0L))
})

test_that("add_rownames keeps the tbl classes (#882)", {
  res <- add_rownames( mtcars, "Make&Model" )
  expect_equal( class(res), c("tbl_df","tbl", "data.frame"))
})

test_that("matrix", {
  expect_identical(as_data_frame(diag(3L)),
                   as_data_frame(as.data.frame(diag(3L))))
})

# Validation --------------------------------------------------------------

test_that("2d object isn't a valid column", {
  expect_error(
    check_data_frame(list(x = mtcars)),
    "Each variable must be a 1d atomic vector"
  )
})

test_that("POSIXlt isn't a valid column", {
  expect_error(
    check_data_frame(list(x = as.POSIXlt(Sys.time()))),
    "Date/times must be stored as POSIXct"
  )
})

test_that("NULL isn't a valid column", {
  expect_error(
    check_data_frame(list(a = NULL)),
    "Each variable must be a 1d atomic vector"
  )
})

test_that("columns must be named (#1101)", {
  l <- list(1:10, 1:10)

  expect_error(
    check_data_frame(l),
    "Each variable must be named"
  )

  expect_error(
    check_data_frame(setNames(l, c("x", ""))),
    "Each variable must be named"
  )

  expect_error(
    check_data_frame(setNames(l, c("x", NA))),
    "Each variable must be named"
  )
})

test_that("names must be unique (#820)", {
  expect_error(
    check_data_frame(list(x = 1, x = 2)),
    "Each variable must have a unique name"
  )
})

# add_row ---------------------------------------------------------------

test_that("can add new row", {
  df_all_new <- add_row(df_all, a = 4, b = 3L)
  expect_identical(nrow(df_all_new), nrow(df_all) + 1L)
  expect_identical(df_all_new$a, c(df_all$a, 4))
  expect_identical(df_all_new$b, c(df_all$b, 3L))
  expect_identical(df_all_new$c, c(df_all$c, NA))
})

test_that("error if adding row with unknown variables", {
  expect_error(add_row(data_frame(a = 3), data_frame(xxyzy = "err")))
})
