context("colwise utils")

test_that("tbl_at_vars() errs on bad input", {
  expect_error(
    tbl_at_vars(iris, raw(3)),
    "`.vars` must be a character/numeric vector or a `vars()` object, not a raw vector",
    fixed = TRUE
  )
})

test_that("tbl_at_vars() treats `NULL` as empty inputs", {
  expect_identical(tbl_at_vars(mtcars, vars(NULL)), tbl_at_vars(mtcars, vars()))
  expect_identical(
    tibble::remove_rownames(mutate_at(mtcars, vars(NULL), `*`, 100)),
    tibble::remove_rownames(mtcars)
  )
})

test_that("tbl_if_vars() errs on bad input", {
  expect_error(
    tbl_if_vars(iris, list(identity, force), environment()),
    "`.predicate` must have length 1, not 2",
    fixed = TRUE
  )

  .funs <- list(identity, force)
  .funs <- as_fun_list(.funs, caller_env())
  expect_error(
    tbl_if_vars(iris, .funs, environment()),
    "`.predicate` must have length 1, not 2",
    fixed = TRUE
  )
})

test_that("lists of formulas are auto-named", {
  df <- tibble(x = 1:3, y = 4:6)

  out <- df %>% summarise_all(list(~ mean(.), ~sd(.x, na.rm = TRUE)))
  expect_named(out, c("x_mean", "y_mean", "x_sd", "y_sd"))

  out <- df %>% summarise_all(list(foobar = ~ mean(.), ~sd(.x, na.rm = TRUE)))
  expect_named(out, c("x_foobar", "y_foobar", "x_sd", "y_sd"))
})
