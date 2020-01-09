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

  df <- tibble(x = 1)
  copy_to(src_env, df, name = "x", overwrite = TRUE)
  expect_equal(env$x, df)
})

test_that("src_local() gives meaningful error messages", {
  verify_output(test_path("test-deprec-src-local-errors.txt"), {

    "# src_local errs with pkg/env"
    src_df("base", new.env())
    src_df()

    "# copy_to"
    env <- new.env(parent = emptyenv())
    env$x <- 1
    src_env <- src_df(env = env)
    copy_to(src_env, tibble(x = 1), name = "x")
  })
})
