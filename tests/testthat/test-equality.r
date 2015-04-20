context("Equality")

# A data frame with all major types
df_all <- data.frame(
  a = c(1, 2.5),
  b = 1:2,
  c = c(T, F),
  d = c("a", "b"),
  e = factor(c("a", "b")),
  f = Sys.Date() + 1:2,
  g = Sys.time() + 1:2,
  stringsAsFactors = FALSE
)

test_that("data frames equal to themselves", {
  expect_true(all.equal(tbl_df(mtcars), tbl_df(mtcars)))
  expect_true(all.equal(tbl_df(iris), tbl_df(iris)))
  expect_true(all.equal(tbl_df(df_all), tbl_df(df_all)))
})

test_that("data frames equal to random permutations of themselves", {
  scramble <- function(x){
    x[sample(nrow(x)), sample(ncol(x)), drop = FALSE]
  }

  expect_equal(tbl_df(mtcars), tbl_df(scramble(mtcars)))
  expect_equal(tbl_df(iris), tbl_df(scramble(iris)))
  expect_equal(tbl_df(df_all), tbl_df(scramble(df_all)))
})

test_that("data frames not equal if missing row", {
  expect_match(all.equal(tbl_df(mtcars), mtcars[-1, ]), "Rows in x but not y: 1")
  expect_match(all.equal(tbl_df(iris), iris[-1, ]),     "Rows in x but not y: 1")
  expect_match(all.equal(tbl_df(df_all), df_all[-1, ]), "Rows in x but not y: 1")
})

test_that("data frames not equal if missing col", {
  expect_match(all.equal(tbl_df(mtcars), mtcars[, -1]), "Cols in x but not y: mpg")
  expect_match(all.equal(tbl_df(iris), iris[, -1]),     "Cols in x but not y: Sepal.Length")
  expect_match(all.equal(tbl_df(df_all), df_all[, -1]), "Cols in x but not y: a")
})

test_that("factors equal only if levels equal", {
  df1 <- data.frame(x = factor(c("a", "b")))
  df2 <- data.frame(x = factor(c("a", "d")))
  expect_match(all.equal(tbl_df(df1), tbl_df(df2)), "Factor levels not equal for column x" )
})

test_that("integers and reals are not equal", {
  x <- 1:10
  y <- as.numeric(x)

  df1 <- data.frame(x = x)
  df2 <- data.frame(x = y)

  expect_match(all.equal(tbl_df(df1), df2),
    "Incompatible type for column x: x integer, y numeric")
})

test_that("BoolResult does not overwrite singleton R_TrueValue", {
  dplyr:::equal_data_frame(mtcars, mtcars)
  expect_equal( class(2 == 2), "logical" )
})

test_that("all.equal.data.frame handles data.frames with NULL names", {
  x <- data.frame(LETTERS[1:3], rnorm(3))
  names(x) <- NULL
  expect_true(all.equal(x,x))
})

test_that( "data frame equality test with ignore_row_order=TRUE detects difference in number of rows. #1065", {
  DF1 <- data_frame(a = 1:4, b = letters[1:4])
  DF2 <- data_frame(a = c(1:4,4L), b = letters[c(1:4,4L)])
  expect_false( isTRUE(all.equal(DF1, DF2, ignore_row_order=TRUE)))
})
