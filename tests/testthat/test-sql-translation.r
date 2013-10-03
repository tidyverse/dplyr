context("SQL translation")

test <- src_sqlite(tempfile(), create = TRUE)

expect_same_in_sql <- function(expr) {
  expr <- substitute(expr)
  
  sql <- translate_sql_q(list(expr))
  actual <- qry_fetch(test$con, paste0("SELECT ", sql))[[1]]
  
  exp <- eval(expr, parent.frame())
  
  expect_equal(actual, exp, label = deparse(substitute(expr)))
}

test_that("Simple maths is correct", {
  expect_same_in_sql(1 + 2)
  expect_same_in_sql(2 * 4)
  expect_same_in_sql(5 / 10)
  expect_same_in_sql(1 - 10)
  expect_same_in_sql(5 ^ 2)
  expect_same_in_sql(5 ^ 1/2)
  expect_same_in_sql(100 %% 3)
})
