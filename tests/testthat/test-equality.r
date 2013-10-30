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
  expect_true(equal_data_frame(mtcars, mtcars))
  expect_true(equal_data_frame(iris, iris))
  expect_true(equal_data_frame(df_all, df_all))
})

test_that("data frames equal to random permutations of themselves", {
  scramble <- function(x){
    x[sample(nrow(x)), sample(ncol(x)), drop = FALSE]
  }
  
  expect_true(equal_data_frame(mtcars, scramble(mtcars)))
  expect_true(equal_data_frame(iris, scramble(iris)))
  expect_true(equal_data_frame(df_all, scramble(df_all)))
})

test_that("data frames not equal if missing row", {
  expect_false(equal_data_frame(mtcars, mtcars[-1, ]))
  expect_false(equal_data_frame(iris, iris[-1, ]))
  expect_false(equal_data_frame(df_all, df_all[-1, ]))
})

test_that("factors equal only if levels equal", {
  df1 <- data.frame(x = factor(c("a", "b")))
  df2 <- data.frame(x = factor(c("a", "d")))
  expect_false(equal_data_frame(df1, df2))
})

test_that("integers and reals are not equal", {
  x <- 1:10
  y <- as.numeric(x)
  
  df1 <- data.frame(x = x)
  df2 <- data.frame(x = y)
  
  expect_false(equal_data_frame(x, y))
})