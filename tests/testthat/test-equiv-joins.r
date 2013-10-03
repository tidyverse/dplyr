context("Equivalence (joins)") 

c <- data.frame(
  x = c(1, 1, 2, 3), 
  y = c(1, 1, 2, 3), 
  a = 1:4)
d <- data.frame(
  x = c(1, 2, 2, 4), 
  y = c(1, 2, 2, 4),
  b = 1:4)

c_tbls <- clone_tbls(c)
d_tbls <- clone_tbls(d)

test_that("inner join equivalent across all tbls", {
  join1 <- function(x, y) {
    strip(inner_join(x, y, by = c("x", "y")), order = TRUE)
  }
  j <- Map(join1, c_tbls, d_tbls)
  
  expect_equal(j$dt, j$df)
  expect_equal(j$sqlite, j$df)
  expect_equal(j$postgres, j$df)
})

test_that("left join equivalent across all tbls", {
  join1 <- function(x, y) {
    strip(left_join(x, y, by = c("x", "y")), order = TRUE)
  }
  j <- Map(join1, c_tbls, d_tbls)
  
  expect_equal(j$dt, j$df)
  expect_equal(j$sqlite, j$df)
  expect_equal(j$postgres, j$df)
})

test_that("semi join equivalent across all tbls", {
  join1 <- function(x, y) {
    strip(semi_join(x, y, by = c("x", "y")), order = TRUE)
  }
  j <- Map(join1, c_tbls, d_tbls)
  
  expect_equal(j$dt, j$df)
  expect_equal(j$sqlite, j$df)
  expect_equal(j$postgres, j$df)
  
})

test_that("anti join equivalent across all tbls", {
  join1 <- function(x, y) {
    strip(anti_join(x, y, by = c("x", "y")), order = TRUE)
  }
  j <- Map(join1, c_tbls, d_tbls)
  
  expect_equal(j$dt, j$df)
  expect_equal(j$sqlite, j$df)
  expect_equal(j$postgres, j$df)
})
