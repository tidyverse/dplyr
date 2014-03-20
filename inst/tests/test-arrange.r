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
    function(x) x %.% arrange(b) %.% arrange(a))
})

test_that("arrange results same regardless of backend", {
  # Can't check db because types are not currently preserved
  tbls <- temp_load(local, df2)

  compare_tbls(tbls, function(x) x %.% arrange(a, id), compare = equal_df)
  compare_tbls(tbls, function(x) x %.% arrange(b, id), compare = equal_df)
  compare_tbls(tbls, function(x) x %.% arrange(c, id), compare = equal_df)
  compare_tbls(tbls, function(x) x %.% arrange(d, id), compare = equal_df)

  compare_tbls(tbls, function(x) x %.% arrange(desc(a), id), compare = equal_df)
  compare_tbls(tbls, function(x) x %.% arrange(desc(b), id), compare = equal_df)
  compare_tbls(tbls, function(x) x %.% arrange(desc(c), id), compare = equal_df)
  compare_tbls(tbls, function(x) x %.% arrange(desc(d), id), compare = equal_df)
})

test_that("arrange uses the white list", {
  env <- environment()
  Period <- setClass("Period", contains = "numeric", where = env)
  on.exit(removeClass("Period", where = env))

  df <- data.frame( p = Period(c(1, 2, 3)), x = 1:3 )
  expect_error(arrange(df, p))

})

test_that("arrange handles list columns (#282)", {
  df <- data.frame( a = 2:1 )
  df$b <- list( "foo", "bar" )
  res <- arrange(df, a)
  expect_equal(res$b, list( "bar", "foo" ) )
})

test_that("arrange handles the case where ... is missing (#338)",{
  expect_equal(arrange(mtcars), mtcars)
})

test_that("arrange handles 0-rows data frames", {
  d <- data.frame(a = numeric(0))
  expect_equal(d, arrange(d))
})

