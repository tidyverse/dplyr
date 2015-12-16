context("src")

test_that("local and env sources", {
  local <- src_local(identity, pkg = "datasets")
  env <- src_df(env = as.environment(list(cars = mtcars, flowers = iris)))

  expect_identical(tbl(local, "iris"), iris)
  expect_match(format(env),
               "src:  <environment: .*>\ntbls: cars, flowers")
})
