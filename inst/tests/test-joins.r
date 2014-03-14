context("Joins")

# Univariate keys --------------------------------------------------------------

a <- data.frame(x = c(1, 1, 2, 3), y = 1:4)
b <- data.frame(x = c(1, 2, 2, 4), z = 1:4)

test_that("univariate inner join has all columns, repeated matching rows", {
  j <- inner_join(a, b, "x")
  
  expect_equal(names(j), c("x", "y", "z"))
  expect_equal(j$y, c(1, 2, 3, 3))
  expect_equal(j$z, c(1, 1, 2, 3))
})

test_that("univariate left join has all columns, all rows", {
  j1 <- left_join(a, b, "x")
  j2 <- left_join(b, a, "x")
  
  expect_equal(names(j1), c("x", "y", "z"))
  expect_equal(names(j2), c("x", "z", "y"))
  
  expect_equal(j1$z, c(1, 1, 2, 3, NA))
  expect_equal(j2$y, c(1, 2, 3, 3, NA))
})

test_that("univariate semi join has x columns, matching rows", {
  j1 <- semi_join(a, b, "x")
  j2 <- semi_join(b, a, "x")
  
  expect_equal(names(j1), c("x", "y"))
  expect_equal(names(j2), c("x", "z"))
  
  expect_equal(j1$y, 1:3)
  expect_equal(j2$z, 1:3)
})

test_that("univariate anti join has x columns, missing rows", {
  j1 <- anti_join(a, b, "x")
  j2 <- anti_join(b, a, "x")
  
  expect_equal(names(j1), c("x", "y"))
  expect_equal(names(j2), c("x", "z"))
  
  expect_equal(j1$y, 4)
  expect_equal(j2$z, 4)
})

# Bivariate keys ---------------------------------------------------------------

c <- data.frame(
  x = c(1, 1, 2, 3), 
  y = c(1, 1, 2, 3), 
  a = 1:4)
d <- data.frame(
  x = c(1, 2, 2, 4), 
  y = c(1, 2, 2, 4),
  b = 1:4)

test_that("bivariate inner join has all columns, repeated matching rows", {
  j <- inner_join(c, d, c("x", "y"))
  
  expect_equal(names(j), c("x", "y", "a", "b"))
  expect_equal(j$a, c(1, 2, 3, 3))
  expect_equal(j$b, c(1, 1, 2, 3))
})

test_that("bivariate left join has all columns, all rows", {
  j1 <- left_join(c, d, c("x", "y"))
  j2 <- left_join(d, c, c("x", "y"))
  
  expect_equal(names(j1), c("x", "y", "a", "b"))
  expect_equal(names(j2), c("x", "y", "b", "a"))
  
  expect_equal(j1$b, c(1, 1, 2, 3, NA))
  expect_equal(j2$a, c(1, 2, 3, 3, NA))
})

test_that("bivariate semi join has x columns, matching rows", {
  j1 <- semi_join(c, d, c("x", "y"))
  j2 <- semi_join(d, c, c("x", "y"))
  
  expect_equal(names(j1), c("x", "y", "a"))
  expect_equal(names(j2), c("x", "y", "b"))
  
  expect_equal(j1$a, 1:3)
  expect_equal(j2$b, 1:3)
})

test_that("bivariate anti join has x columns, missing rows", {
  j1 <- anti_join(c, d, c("x", "y"))
  j2 <- anti_join(d, c, c("x", "y"))
  
  expect_equal(names(j1), c("x", "y", "a"))
  expect_equal(names(j2), c("x", "y", "b"))
  
  expect_equal(j1$a, 4)
  expect_equal(j2$b, 4)
})


# Duplicate column names --------------------------------------------------

e <- data.frame(x = c(1, 1, 2, 3), z = 1:4)
f <- data.frame(x = c(1, 2, 2, 4), z = 1:4)

test_that("univariate inner join has all columns, repeated matching rows", {
  j <- inner_join(e, f, "x")
  
  expect_equal(names(j), c("x", "z.x", "z.y"))
  expect_equal(j$z.x, c(1, 2, 3, 3))
  expect_equal(j$z.y, c(1, 1, 2, 3))
})

test_that("univariate left join has all columns, all rows", {
  j1 <- left_join(e, f, "x")
  j2 <- left_join(f, e, "x")
  
  expect_equal(names(j1), c("x", "z.x", "z.y"))
  expect_equal(names(j2), c("x", "z.x", "z.y"))
  
  expect_equal(j1$z.y, c(1, 1, 2, 3, NA))
  expect_equal(j2$z.y, c(1, 2, 3, 3, NA))
})

test_that("inner_join does not segfault on NA in factors (#306)", {
  a <- data.frame(x=c("p", "q", NA), y=c(1, 2, 3), stringsAsFactors=TRUE)
  b <- data.frame(x=c("p", "q", "r"), z=c(4,5,6), stringsAsFactors=TRUE)
  res <- inner_join(a, b)
  expect_equal( nrow(res), 2L )
})

