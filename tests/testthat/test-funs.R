context("funs")

test_that("fun_list is merged with new args", {
  funs <- funs(fn = bar)
  funs <- as_fun_list(funs, quo(bar), env(), baz = "baz")
  expect_identical(funs$fn, quo(bar(., baz = "baz")))
})

test_that("funs() works with namespaced calls", {
  expect_identical(summarise_all(mtcars, funs(base::mean(.))), summarise_all(mtcars, funs(mean(.))))
  expect_identical(summarise_all(mtcars, funs(base::mean)), summarise_all(mtcars, funs(mean(.))))
})

test_that("funs() accepts quoted functions", {
  expect_identical(funs(mean), funs("mean"))
})

test_that("funs() accepts unquoted functions", {
  funs <- funs(fn = !! mean)
  expect_identical(funs$fn, new_quosure(lang(base::mean, quote(.))))
})

test_that("funs() accepts quoted calls", {
  expect_identical(funs(mean), funs(mean(.)))
})

test_that("funs() can be merged with new arguments", {
  fns <- funs(foo(.))
  expect_identical(as_fun_list(fns, ~NULL, get_env(), foo = 1L), funs(foo(., foo = 1L)))
})


enfun <- function(.funs, ...) {
  as_fun_list(.funs, enquo(.funs), caller_env(), ...)
}

test_that("can enfun() literal functions", {
  expect_identical(enfun(identity(mean)), funs(!! mean))
})

test_that("can enfun() named functions by expression", {
  expect_identical(enfun(mean), funs(mean(.)))
})

test_that("local objects are not treated as symbols", {
  mean <- funs(my_mean(.))
  expect_identical(enfun(mean), mean)
})

test_that("can enfun() character vectors", {
  expect_identical(enfun(c("min", "max")), funs(min, max))
})

test_that("can enfun() quosures", {
  expect_identical(enfun(quo(mean(.))), funs(mean(.)))
})

test_that("can enfun() purrr-style lambdas", {
  my_mean <- as_function(~mean(.x))
  expect_identical(enfun(~mean(.x)), funs(!! my_mean))
})
