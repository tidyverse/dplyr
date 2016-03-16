context("SQL: build")

test_that("base source of lazy frame is always 'df'", {
  out <- lazy_frame(x = 1, y = 5) %>% sql_build()
  expect_equal(out, ident("df"))
})

# select and rename -------------------------------------------------------

test_that("select picks variables", {
  out <- lazy_frame(x1 = 1, x2 = 1, x3 = 2) %>%
    select(x1:x2) %>%
    sql_build()

  expect_equal(out$select, c("x1" = "x1", "x2" = "x2"))
})

test_that("select renames variables", {
  out <- lazy_frame(x1 = 1, x2 = 1, x3 = 2) %>%
    select(y = x1, z = x2) %>%
    sql_build()

  expect_equal(out$select, c("y" = "x1", "z" = "x2"))
})

test_that("select can refer to variables in local env", {
  vars <- c("x", "y")
  out <- lazy_frame(x = 1, y = 1) %>%
    select(one_of(vars)) %>%
    sql_build()

  expect_equal(out$select, c("x" = "x", "y" = "y"))
})

test_that("rename preserves existing vars", {
  out <- lazy_frame(x = 1, y = 1) %>%
    rename(z = y) %>%
    sql_build()

  expect_equal(out$select, c("x" = "x", "z" = "y"))
})
