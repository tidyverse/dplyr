context("Filter")

df <- expand.grid(a = 1:10, b = letters[1:10],
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)

srcs <- temp_srcs(c("df", "dt", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)

test_that("filter results independent of data tbl (simple)", {
  expected <- df[df$a > 6, , drop = FALSE]
  compare_tbls(tbls, function(x) x %.% filter(a > 6), ref = expected)
})

test_that("filter captures local variables", {
  sel <- c("d", "g", "a")
  expected <- df[df$b %in% sel, , drop = FALSE]

  compare_tbls(tbls, function(x) x %.% filter(b %in% sel), ref = expected)
})

test_that("two filters equivalent to one", {
  expected <- filter(df, a > 4 & b == "a")
  
  compare_tbls(tbls, function(x) x %.% filter(a > 4) %.% filter(b == "a"), 
    ref = expected)
})

test_that("filter fails if inputs incorrect length (#156)", {
  expect_error( filter(tbl_df(mtcars), c(F, T)) )
  expect_error( filter(group_by(mtcars, am), c(F, T)) )
})

test_that("filter fails if no expression is given (#157)", {
  expect_error( filter(mtcars) )
})

test_that("filter gives useful error message when given incorrect input", {
  expect_error( filter(tbl_df(mtcars), x ), "unknown column" )
  expect_error( filter(tbl_df(mtcars), TRUE), "incompatible expression in filter" )
})

test_that("filter handles passing ...", {                        
  df <- data.frame( x = 1:4 )
  
  f <- function(...){
    x1 <- 4
    f1 <- function(y) y
    filter(df, ..., f1(x1) > x)  
  }
  g <- function(...){
    x2 <- 2
    f(x > x2, ...)
  }
  res <- g()
  expect_equal( res$x, 3L )
  
  df <- group_by(df,x)
  res <- g()
  expect_equal( res$x, 3L )
  
})

test_that( "filter handles simple symbols", {
  df <- data.frame( x = 1:4, test = rep(c(T,F), each = 2) )
  res <- filter(df, test) 
  
  gdf <- group_by(df,x)
  res <- filter(gdf, test) 
  
  f <- function(data, ...){
    one <- 1
    filter( data, test, x > one, ...)
  }
  g <- function(data, ...){
    four <- 4
    f( data, x < four, ...)
  }
  res <- g(df)
  expect_equal(res$x, 2L)
  expect_equal(res$test, TRUE)
  
  res <- g(gdf)
  expect_equal(res$x, 2L)
  expect_equal(res$test, TRUE)
  
})

