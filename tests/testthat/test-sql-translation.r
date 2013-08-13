context("SQL translation")

test_con <- dbConnect(dbDriver("SQLite"), 
  dbname = file.path(tempdir(), "test.sqlite3"))
RSQLite.extfuns::init_extensions(test_con)

eval_sql <- function(expr) {
  select <- trans_sqlite(expr, .data, parent.frame())
  sql <- paste0("SELECT ", sql_vars(select))
  exec_sql(test_con, sql)[[1]]
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
