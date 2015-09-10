context("data_frame")

test_that("data_frame returns correct number of rows with all combinatinos", {

  expect_equal(nrow(data_frame(value = 1:10)), 10L)

  expect_equal(nrow(data_frame(value = 1:10, name = "recycle_me")), 10L)

  expect_equal(nrow(data_frame(name = "recycle_me", value = 1:10)), 10L)

  expect_equal(nrow(data_frame(name = "recycle_me", value = 1:10, value2 = 11:20)), 10L)

  expect_equal(nrow(data_frame(value = 1:10, name = "recycle_me", value2 = 11:20)), 10L)

})

test_that("can't make data_frame containing data.frame or array", {
  expect_error(data_frame(mtcars), "only contain 1d atomic vectors and lists")
  expect_error(data_frame(diag(5)), "only contain 1d atomic vectors and lists")
})

test_that("null isn't a valid column", {
  expect_error(data_frame(a = NULL), "only contain 1d atomic vectors and lists")
  expect_error(as_data_frame(list(a = NULL)), "only contain 1d atomic vectors and lists")
})

# as_data_frame -----------------------------------------------------------

test_that("columns must be same length", {
  l <- list(x = 1, y = 1:2)
  expect_error(as_data_frame(l), "not all same length")
})

test_that("columns must be named", {
  l1 <- list(1:10)
  l2 <- list(x = 1, 2)

  expect_error(as_data_frame(l1), "must be named")
  expect_error(as_data_frame(l2), "must be named")
})

test_that("can't coerce list data.frame or array", {
  expect_error(as_data_frame(list(x = mtcars)), "only contain 1d atomic vectors and lists")
  expect_error(as_data_frame(list(x = diag(5))), "only contain 1d atomic vectors and lists")
})

test_that("Zero column list makes 0 x 0 tbl_df", {
  zero <- as_data_frame(list())
  expect_is(zero, "tbl_df")
  expect_equal(dim(zero), c(0L, 0L))
})

test_that("error on NA column names (#1101)", {
  df <- data.frame( x = 1:10, y = 1:10 )
  names(df)[1] <- NA
  expect_error( as_data_frame(df), "All columns must be named" )
})

test_that( "add_rownames keeps the tbl classes (#882)", {
    res <- mtcars %>% add_rownames( "Make&Model" )
    expect_equal( class(res), c("tbl_df","tbl", "data.frame"))
})
