context("data_frame")

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
  expect_equal(names(df), c("x", "y"))
})

test_that("empty input makes 0 x 0 tbl_df", {
  zero <- data_frame()
  expect_is(zero, "tbl_df")
  expect_equal(dim(zero), c(0L, 0L))
})

# as_data_frame -----------------------------------------------------------

test_that("empty list() makes 0 x 0 tbl_df", {
  zero <- as_data_frame(list())
  expect_is(zero, "tbl_df")
  expect_equal(dim(zero), c(0L, 0L))
})

test_that("add_rownames keeps the tbl classes (#882)", {
  expect_warning(
    res <- mtcars %>% add_rownames( "Make&Model" ),
    "Deprecated"
  )
  expect_equal( class(res), c("tbl_df","tbl", "data.frame"))
})

test_that("data frame column names are automatically generated (#1606)", {
  m <- matrix( 1:6, nr = 3)
  df <- tbl_df(m)
  expect_equal( names(df), c("V1", "V2") )

  row.names(m) <- 1:3
  df <- tbl_df(m)
  expect_equal( names(df), c("V1", "V2") )

  colnames(m) <- c("a", "b")
  df <- tbl_df(m)
  expect_equal( names(df), c("a", "b") )
})
