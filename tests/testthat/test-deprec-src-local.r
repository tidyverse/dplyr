setup(options(lifecycle_verbosity = "quiet"))
teardown(options(lifecycle_verbosity = NULL))


test_that("src_tbls() includes all tbls (#4326)", {
  expect_equal(
    src_tbls(src_df(env = env(. = iris))),
    "."
  )
})

test_that("src_local only overwrites if overwrite = TRUE", {
  env <- new.env(parent = emptyenv())
  env$x <- 1

  src_env <- src_df(env = env)

  expect_error(
    copy_to(src_env, tibble(x = 1), name = "x"),
    "object with `name` = `x` must not already exist, unless `overwrite` = TRUE",
    fixed = TRUE
  )

  df <- tibble(x = 1)
  copy_to(src_env, df, name = "x", overwrite = TRUE)
  expect_equal(env$x, df)
})

test_that("src_local errs with pkg/env", {
  expect_error(
    src_df("base", new.env()),
    "Exactly one of `pkg` and `env` must be non-NULL, not 2",
    fixed = TRUE
  )

  expect_error(
    src_df(),
    "Exactly one of `pkg` and `env` must be non-NULL, not 0",
    fixed = TRUE
  )
})
