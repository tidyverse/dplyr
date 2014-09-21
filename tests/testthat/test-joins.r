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

test_that("joins don't reorder columns #328", {
  a <- data.frame(a=1:3)
  b <- data.frame(a=1:3, b=1, c=2, d=3, e=4, f=5)
  res <- left_join(a,b)
  expect_equal( names(res), names(b) )
})

test_that("join handles type promotions #123", {
  df <- data.frame(
    V1 = c(rep("a",5), rep("b",5)),
    V2 = rep(c(1:5), 2),
    V3 = c(101:110),
    stringsAsFactors = FALSE
  )

  match <- data.frame(
    V1 = c("a", "b"),
    V2 = c(3.0, 4.0),
    stringsAsFactors = FALSE
  )
  res <- semi_join(df, match)
  expect_equal( res$V2, 3:4 )
  expect_equal( res$V3, c(103L, 109L) )

  df1 <- data.frame( a = c("a", "b" ), b = 1:2, stringsAsFactors = TRUE )
  df2 <- data.frame( a = c("a", "b" ), c = 4:5, stringsAsFactors = FALSE )
  res <- semi_join( df1, df2 )
  res <- semi_join( df2, df1 )

})

test_that("indices don't get mixed up when nrow(x) > nrow(y). #365",{
  a <- data.frame(V1 = c(0, 1, 2), V2 = c("a", "b", "c"), stringsAsFactors = FALSE)
  b <- data.frame(V1 = c(0, 1), V3 = c("n", "m"), stringsAsFactors = FALSE)
  res <- inner_join(a, b, by = "V1")
  expect_equal( res$V1, c(0,1) )
  expect_equal( res$V2, c("a", "b"))
  expect_equal( res$V3, c("n", "m"))
})

test_that("join functions error on column not found #371", {
  expect_error(
    left_join(data.frame(x=1:5), data.frame(y=1:5), by="x"),
    "cannot join on columns 'x'"
  )
  expect_error(
    left_join(data.frame(x=1:5), data.frame(y=1:5), by="y"),
    "cannot join on columns 'y'"
  )
  expect_error(
    left_join(data.frame(x=1:5), data.frame(y=1:5)),
    "No common variables"
  )
})

test_that("joining data tables yields a data table (#470)", {
  out <- left_join(data.table(a), data.table(b))
  expect_is(out, "data.table")
  out <- semi_join(data.table(a), data.table(b))
  expect_is(out, "data.table")
})

test_that("inner_join is symmetric (even when joining on character & factor)", {
  foo <- data_frame(id = factor(c("a", "b")), var1 = "foo")
  bar <- data_frame(id = c("a", "b"), var2 = "bar")

  tmp1 <- inner_join(foo, bar, by="id")
  tmp2 <- inner_join(bar, foo, by="id")
  
  expect_is(tmp1$id, "character")
  expect_is(tmp2$id, "character")
  
  expect_equal(names(tmp1), c("id", "var1", "var2"))
  expect_equal(names(tmp2), c("id", "var2", "var1"))

  expect_equal(tmp1, tmp2)
})

test_that("inner_join is symmetric, even when type of join var is different (#450)", {
  foo <- tbl_df(data.frame(id = 1:10, var1 = "foo"))
  bar <- tbl_df(data.frame(id = as.numeric(rep(1:10, 5)), var2 = "bar"))

  tmp1 <- inner_join(foo, bar, by="id")
  tmp2 <- inner_join(bar, foo, by="id")

  expect_equal(names(tmp1), c("id", "var1", "var2"))
  expect_equal(names(tmp2), c("id", "var2", "var1"))

  expect_equal(tmp1, tmp2)
})

