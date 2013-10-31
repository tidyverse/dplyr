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
  expect_true(all.equal(mtcars, mtcars))
  expect_true(all.equal(iris, iris))
  expect_true(all.equal(df_all, df_all))
})

test_that("data frames equal to random permutations of themselves", {
  scramble <- function(x){
    x[sample(nrow(x)), sample(ncol(x)), drop = FALSE]
  }
  
  expect_true(all.equal(mtcars, scramble(mtcars)))
  expect_true(all.equal(iris, scramble(iris)))
  expect_true(all.equal(df_all, scramble(df_all)))
})

test_that("data frames not equal if missing row", {
  expect_match(all.equal(mtcars, mtcars[-1, ]), "Rows in x, but not in y : 1\n")
  expect_match(all.equal(iris, iris[-1, ]), "Rows in x, but not in y : 1\n")
  expect_match(all.equal(df_all, df_all[-1, ]), "Rows in x, but not in y : 1\n")
})

test_that("factors equal only if levels equal", {
  df1 <- data.frame(x = factor(c("a", "b")))
  df2 <- data.frame(x = factor(c("a", "d")))
  expect_match(all.equal(df1, df2), "Levels mismatch for column x" )
})

test_that("integers and reals are not equal", {
  x <- 1:10
  y <- as.numeric(x)
  
  df1 <- data.frame(x = x)
  df2 <- data.frame(x = y)
  
  expect_match(all.equal(df1, df2), "Incompatible type for column x: x integer, y numeric\n" )
})