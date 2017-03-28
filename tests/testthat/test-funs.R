context("funs")

test_that("fun_list is merged with new args", {
  funs <- funs(fn = bar)
  funs <- as_fun_list(funs, ~bar, baz = "baz")
  expect_identical(funs$fn, ~bar(., baz = "baz"))
})

test_that("funs() works with namespaced calls", {
  expect_identical(summarise_all(mtcars, funs(base::mean(.))), summarise_all(mtcars, funs(mean(.))))
  expect_identical(summarise_all(mtcars, funs(base::mean)), summarise_all(mtcars, funs(mean(.))))
})
