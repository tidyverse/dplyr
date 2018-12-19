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
  funs <- funs(fn = !!mean)
  expect_identical(funs$fn, new_quosure(call2(base::mean, quote(.))))
})

test_that("funs() accepts quoted calls", {
  expect_identical(funs(mean), funs(mean(.)))
})

test_that("funs() gives a clear error message (#3368)", {
  expect_error(
    funs(function(si) { mp[si] }),
    glue("`function(si) {{
             mp[si]
         }}` must be a function name (quoted or unquoted) or an unquoted call, not `function`"),
    fixed = TRUE
  )

  expect_error(
    funs(~mp[.]),
    "`~mp[.]` must be a function name (quoted or unquoted) or an unquoted call, not `~`",
    fixed = TRUE
  )
})

test_that("funs() can be merged with new arguments", {
  fns <- funs(foo(.))
  expect_identical(as_fun_list(fns, ~ NULL, current_env(), foo = 1L), funs(foo(., foo = 1L)))
})


enfun <- function(.funs, ...) {
  as_fun_list(.funs, enquo(.funs), caller_env(), ...)
}

test_that("can enfun() literal functions", {
  expect_identical(enfun(identity(mean)), funs(!!mean))
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
  my_mean <- as_function(~ mean(.x))
  expect_identical(enfun(~ mean(.x)), funs(!!my_mean))
})

test_that("as_fun_list() uses rlang auto-naming", {
  nms <- names(as_fun_list(list(min, max), quo(), env()))

  # Just check they are labellised as literals enclosed in brackets to
  # insulate from upstream changes
  expect_true(all(grepl("^<", nms)))
})

test_that("funs_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    funs(mean),
    funs_(list(~ mean))
  )

  expect_equal(
    funs_(list("mean")),
    funs_(list(`environment<-`(~ mean, baseenv())))
  )

  expect_equal(
    funs(mean(.)),
    funs_(list(~ mean(.)))
  )
})
