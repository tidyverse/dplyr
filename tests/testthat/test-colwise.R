context("colwise utils")

test_that("tbl_at_vars() errs on bad input", {
  expect_error(
    tbl_at_vars(iris, raw(3)),
    "`.vars` must be a character/numeric vector or a `vars()` object, not raw",
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
    tbl_if_vars(iris, funs(identity, force), environment()),
    "`.predicate` must have length 1, not 2",
    fixed = TRUE
  )
})
