test_that("src_tbls() includes all tbls (#4326)", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    src_tbls(src_df(env = env(. = iris))),
    "."
  )
})

test_that("src_local only overwrites if overwrite = TRUE", {
  withr::local_options(lifecycle_verbosity = "quiet")

  env <- new.env(parent = emptyenv())
  env$x <- 1

  src_env <- src_df(env = env)

  df <- tibble(x = 1)
  copy_to(src_env, df, name = "x", overwrite = TRUE)
  expect_equal(env$x, df)
})

test_that("src_df() is deprecated / errors", {
  withr::local_options(lifecycle_verbosity = "quiet")

  # src_local errs with pkg/env
  expect_snapshot_error(src_df("base", new.env()))
  expect_snapshot_error(src_df())

  env <- new.env(parent = emptyenv())
  env$x <- 1
  src_env <- src_df(env = env)
  expect_snapshot_error(
    copy_to(src_env, tibble(x = 1), name = "x")
  )
})
