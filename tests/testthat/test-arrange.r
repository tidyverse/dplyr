context("Arrange")

df1 <- expand.grid(
  a = sample(letters, 5),
  b = sample(letters, 5),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)
tbls <- clone_tbls(df1)

test_that("two arranges equivalent to one", {
  exp <- strip(arrange(df1, a, b))
  
  expect_equal(strip(arrange(arrange(tbls$df, b), a)), exp)
  expect_equal(strip(arrange(arrange(tbls$dt, b), a)), exp)
  expect_equal(strip(arrange(arrange(tbls$sqlite, b), a)), exp)
  expect_equal(strip(arrange(arrange(tbls$postgres, b), a)), exp)
})

df2 <- data.frame( 
  a = rep(c(NA, 1, 2, 3), each = 4), 
  b = rep(c(0L, NA, 1L, 2L), 4), 
  c = c(NA, NA, NA, NA, letters[10:21]),
  d = rep( c(T, NA, F, T), each = 4),
  stringsAsFactors = FALSE
)

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
