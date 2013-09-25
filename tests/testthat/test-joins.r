context("Joins")

a <- data.frame(x = c(1, 1, 2, 3), y = 1:4)
b <- data.frame(x = c(1, 2, 2, 4), z = 1:4)

test_that("simple inner join has all columns, repeated matching rows", {
  j <- inner_join(a, b, "x")
  
  expect_equal(names(j), c("x", "y", "z"))
  expect_equal(j$y, c(1, 2, 3, 3))
  expect_equal(j$z, c(1, 1, 2, 3))
})

test_that("simple left join has all columns, all rows", {
  j1 <- left_join(a, b, "x")
  j2 <- left_join(b, a, "x")
  
  expect_equal(names(j1), c("x", "y", "z"))
  expect_equal(names(j2), c("x", "z", "y"))
  
  expect_equal(j1$z, c(1, 1, 2, 3, NA))
  expect_equal(j2$y, c(1, 2, 3, 3, NA))
})

test_that("simple semi join has x columns, matching rows", {
  j1 <- semi_join(a, b, "x")
  j2 <- semi_join(b, a, "x")
  
  expect_equal(names(j1), c("x", "y"))
  expect_equal(names(j2), c("x", "z"))
  
  expect_equal(j1$y, 1:3)
  expect_equal(j2$z, 1:3)
})

test_that("simple semi join, has x columns, missing rows", {
  j1 <- anti_join(a, b, "x")
  j2 <- anti_join(b, a, "x")
  
  expect_equal(names(j1), c("x", "y"))
  expect_equal(names(j2), c("x", "z"))
  
  expect_equal(j1$y, 4)
  expect_equal(j2$z, 4)
})
