context("SQL translation")

test <- src_sqlite(tempfile(), create = TRUE)

eval_sql <- function(expr) {
  select <- trans_sqlite(list(expr), .data, parent.frame())
  sql <- build_sql("SELECT ", select)
  fetch_sql_df(test$con, sql)[[1]]
}

expect_same_in_sql <- function(expr) {
  r <- expr
  sql <- eval_sql(substitute(expr))
  expect_equal(sql, r, label = deparse(substitute(expr)))
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
