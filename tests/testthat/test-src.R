context("src")

test_that("local and env sources", {
  local <- src_local(identity, pkg = "datasets")
  env <- src_df(env = as.environment(list(cars = mtcars, flowers = iris)))

  expect_identical(tbl(local, "iris"), iris)
  expect_match(paste(capture.output(print(env, width = 40L)), collapse = "\n"),
               "src:  <environment: .*>\ntbls: cars, flowers")

  expect_error(src_df(), "exactly one of")
  expect_error(src_df(pkg = "datasets", env = new.env()), "exactly one of")

  expect_true(is.src(local))
  expect_false(is.src(5))
})

test_that("same_src", {
  expect_true(same_src(iris, mtcars))
  expect_false(same_src(iris, NULL))
})
