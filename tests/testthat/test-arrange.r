context("Arrange")

local <- c("df", "dt")
db <- c("sqlite", "postgres")

df1 <- expand.grid(
  a = sample(letters, 5),
  b = sample(letters, 5),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)

df2 <- data.frame(
  a = rep(c(NA, 1, 2, 3), each = 4),
  b = rep(c(0L, NA, 1L, 2L), 4),
  c = c(NA, NA, NA, NA, letters[10:21]),
  d = rep( c(T, NA, F, T), each = 4),
  id = 1:16,
  stringsAsFactors = FALSE
)

equal_df <- function(x, y) {
  rownames(x) <- NULL
  rownames(y) <- NULL
  isTRUE(all.equal(x, y))
}

test_that("local arrange sorts missing values to end", {
  na_last <- function(x) {
    n <- length(x)
    all(is.na(x[(n - 3):n]))
  }

  # Numeric
  expect_true(na_last(arrange(df2, a)$a))
  expect_true(na_last(arrange(df2, desc(a))$a))

  # Integer
  expect_true(na_last(arrange(df2, b)$b))
  expect_true(na_last(arrange(df2, desc(b))$b))

  # Character
  expect_true(na_last(arrange(df2, c)$c))
  expect_true(na_last(arrange(df2, desc(c))$c))

  # Logical
  expect_true(na_last(arrange(df2, d)$d))
  expect_true(na_last(arrange(df2, desc(d))$d))
})

test_that("two arranges equivalent to one", {
  single <- arrange(df1, a, b)

  tbls <- temp_load(c(local, db), df1)
  compare_tbls(tbls, ref = single, compare = equal_df,
    function(x) x %>% arrange(b) %>% arrange(a))
})

test_that("arrange results same regardless of backend", {
  # Can't check db because types are not currently preserved
  tbls <- temp_load(local, df2)

  compare_tbls(tbls, function(x) x %>% arrange(a, id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(b, id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(c, id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(d, id), compare = equal_df)

  compare_tbls(tbls, function(x) x %>% arrange(desc(a), id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(desc(b), id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(desc(c), id), compare = equal_df)
  compare_tbls(tbls, function(x) x %>% arrange(desc(d), id), compare = equal_df)
})

test_that("arrange handles list columns (#282)", {
  df <- data.frame( a = 2:1 )
  df$b <- list( "foo", "bar" )
  res <- arrange(df, a)
  expect_equal(res$b, list( "bar", "foo" ) )
})

test_that("arrange handles the case where ... is missing (#338)",{
  expect_equivalent(arrange(mtcars), mtcars)
})

test_that("arrange handles 0-rows data frames", {
  d <- data.frame(a = numeric(0))
  expect_equal(d, arrange(d))
})

test_that("arrange implements special case (#369)", {
  d1 <- mtcars %>% group_by(cyl,disp) %>% arrange() %>% as.data.frame()
  d2 <- mtcars %>% arrange(cyl, disp)

  expect_equal(d1, d2)
})

test_that("grouped arrange sorts first by group (#491)", {
  df1 <- mtcars %>% group_by(cyl) %>% arrange(disp) %>% ungroup()
  df2 <- mtcars %>% arrange(cyl, disp) %>% tbl_df()

  expect_equal(df1, df2)
})

test_that("arrange keeps the grouping structure (#605)", {
  dat <- data_frame(x = 4:1, g = c('b','b','a','a'))
  res <- dat %>% group_by(g) %>% arrange()
  expect_is(res, "grouped_df" )
  expect_false(is.unsorted(res$g))
  expect_equal(res$x, c(2,1,4,3))
  expect_equal(res$g, c("a", "a", "b", "b"))

  res <- dat %>% group_by(g) %>% arrange(x)
  expect_is(res, "grouped_df")
  expect_false(is.unsorted(res$g))
  expect_true(all(summarise(res, sorted = ! is.unsorted(x) )$sorted))
  expect_equal(attr(res,"indices"), list( c(0,1), c(2,3)) )
})

test_that("arrange handles complex vectors", {
  d <- data.frame(x=1:10,y=10:1+2i)
  res <- arrange(d,y)
  expect_equal( res$y, rev(d$y) )
  expect_equal( res$x, rev(d$x) )

  res <- arrange(res, desc(y))
  expect_equal( res$y, d$y )
  expect_equal( res$x, d$x )

  d$y[ c(3,6) ] <- NA
  res <- arrange(d,y)
  expect_true( all(is.na(res$y[9:10])) )

  res <- arrange(d,desc(y))
  expect_true( all(is.na(res$y[9:10])) )

})

test_that("arrange respects attributes #1105", {
  env <- environment()
  Period <- suppressWarnings( setClass("Period", contains = "numeric", where = env) )
  on.exit(removeClass("Period", where = env))

  df <- data.frame( p = Period(c(1, 2, 3)), x = 1:3 )
  res <- arrange(df, p)
  expect_is(res$p, "Period")
})

test_that("arrange works with empty data frame (#1142)", {
  df <- data.frame()
  res <- df %>% arrange
  expect_equal( nrow(res), 0L )
  expect_equal( length(res), 0L )
})

test_that("arrange respects locale (#1280)", {
  df2 <- data_frame( words = c("casa", "\u00e1rbol", "zona", "\u00f3rgano") )

  res <- df2 %>% arrange( words )
  expect_equal( res$words, sort(df2$words) )

  res <- df2 %>% arrange( desc(words) )
  expect_equal( res$words, sort(df2$words, decreasing = TRUE) )

})

test_that("duplicated column name is explicit about which column (#996)", {
    df <- data.frame( x = 1:10, x = 1:10 )
    names(df) <- c("x", "x")
    expect_error( df %>% arrange, "found duplicated column name: x" )

    df <- data.frame( x = 1:10, x = 1:10, y = 1:10, y = 1:10 )
    names(df) <- c("x", "x", "y", "y")
    expect_error( df %>% arrange, "found duplicated column name: x, y" )
})
