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
    over(ident("x"), order = c("x", "y")),
    sql('"x" OVER (ORDER BY "x", "y")')
  )
  expect_equal(
    over(ident("x"), partition = c("x", "y")),
    sql('"x" OVER (PARTITION BY "x", "y")')
  )
})

test_that("connection affects quoting window function fields", {
  dbiTest <- structure(list(), class = "DBITestConnection")
  dbTest <- src_sql("test", con = dbiTest)
  testTable <- tbl_sql("test", src = dbTest, from = "table1")

  out <- filter(group_by(testTable, field1), min_rank(desc(field1)) < 2)
  sqlText <- sql_render(out)

  testthat::expect_equal(
    grep(paste(
      "^SELECT `field1`",
      "FROM \\(SELECT `field1`, rank\\(\\) OVER \\(PARTITION BY `field1` ORDER BY `field1` DESC\\) AS `[a-zA-Z0-9]+`",
      "FROM `table1`\\) `[a-zA-Z0-9]+`",
      "WHERE \\(`[a-zA-Z0-9]+` < 2.0\\)$",
      sep = "\n"
    ), sqlText),
    1,
    info = sqlText
  )
})
