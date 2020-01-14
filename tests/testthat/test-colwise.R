context("colwise utils")

test_that("tbl_at_vars() treats `NULL` as empty inputs", {
  expect_identical(tbl_at_vars(mtcars, vars(NULL)), tbl_at_vars(mtcars, vars()))
  expect_identical(
    tibble::remove_rownames(mutate_at(mtcars, vars(NULL), `*`, 100)),
    tibble::remove_rownames(mtcars)
  )
})

test_that("lists of formulas are auto-named", {
  df <- tibble(x = 1:3, y = 4:6)

  out <- df %>% summarise_all(list(~ mean(.), ~sd(.x, na.rm = TRUE)))
  expect_named(out, c("x_mean", "y_mean", "x_sd", "y_sd"))

  out <- df %>% summarise_all(list(foobar = ~ mean(.), ~sd(.x, na.rm = TRUE)))
  expect_named(out, c("x_foobar", "y_foobar", "x_sd", "y_sd"))
})

# Errors --------------------------------------------

test_that("colwise utils gives meaningful error messages", {
  verify_output(test_path("test-colwise-errors.txt"), {
    tbl_at_vars(iris, raw(3))
    tbl_if_vars(iris, list(identity, force), environment())

    .funs <- as_fun_list(list(identity, force), caller_env())
    tbl_if_vars(iris, .funs, environment())
  })
})
