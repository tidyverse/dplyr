context("Window functions")

test_that("If n = 0, lead and lag return x", {
  expect_equal(lead(1:2, 0), 1:2)
  expect_equal(lag(1:2, 0), 1:2)
})

test_that("If n = length(x), returns all missing", {
  miss <- rep(NA_integer_, 2)

  expect_equal(lead(1:2, 2), miss)
  expect_equal(lag(1:2, 2), miss)

})

test_that("cumany handles NA (#408)", {
  batman <- c(NA,NA,NA,NA,NA)
  expect_true(all(is.na(cumany(batman))))
  expect_true(all(is.na(cumall(batman))))

  x <- c(FALSE,NA)
  expect_true( all( !cumall(x) ) )

  x <- c(TRUE,NA)
  expect_true( all( cumany(x) ) )

})

test_that("percent_rank ignores NAs (#1132)", {
  expect_equal( percent_rank(c(1:3, NA)), c(0, 0.5, 1, NA) )
})

test_that("cume_dist ignores NAs (#1132)", {
  expect_equal( cume_dist(c(1:3, NA)), c(1/3, 2/3, 1, NA) )
})

test_that( "cummean is not confused by FP error (#1387)", {
  a <- rep(99, 9)
  expect_true( all( cummean(a) == a) )
})

# Databases ---------------------------------------------------------------

test_that("over() only requires first argument", {
  expect_equal(over("X"), sql("'X' OVER ()"))
})

test_that("multiple group by or order values don't have parens", {
  expect_equal(
    over("x", order = c("x", "y")),
    sql("'x' OVER (ORDER BY x, y)")
  )
  expect_equal(
    over("x", partition = c("x", "y")),
    sql("'x' OVER (PARTITION BY x, y)")
  )
})
