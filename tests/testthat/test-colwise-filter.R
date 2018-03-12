context("colwise filter")

test_that("filter_if()", {
  expect_identical(nrow(filter_if(mtcars, is_integerish, all_vars(. > 1))), 0L)
  expect_identical(nrow(filter_if(mtcars, is_integerish, all_vars(. > 0))), 7L)
})

test_that("filter_at()", {
  sepal_large <- filter_at(iris, vars(starts_with("Sepal")), all_vars(. > 4))
  expect_equal(sepal_large$Sepal.Length, c(5.7, 5.2, 5.5))
})

test_that("filter_at can filter by grouping variables (#3351)", {
  tbl <- data_frame(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8) %>%
    group_by(gr1)

  expect_identical(
    filter_at(tbl, vars(gr1), all_vars(.>1)),
    filter(tbl, gr1 > 1)
  )
})

test_that("filter_all()", {
  expect_identical(filter_all(mtcars, any_vars(. > 200))$disp, mtcars$disp[mtcars$disp > 200])
})

test_that("aborts on empty selection", {
  expect_error(
    filter_if(mtcars, is_character, all_vars(. > 0)),
    "`.predicate` has no matching columns",
    fixed = TRUE
  )
})

test_that("aborts when supplied funs()", {
  expect_error(
    filter_all(mtcars, funs(. > 0)),
    "`.vars_predicate` must be a call to `all_vars()` or `any_vars()`, not list",
    fixed = TRUE
  )
})
