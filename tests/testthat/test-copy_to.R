context("copy_to")

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

test_that("auto_copy() requires same source", {
  skip_if_not_installed("dbplyr")

  env <- new.env(parent = emptyenv())
  env$iris <- iris
  src_iris <- src_df(env = env)

  src_mtcars <- src_sqlite(":memory:", create = TRUE)
  copy_to(src_mtcars, mtcars, "mtcars")

  expect_error(
    auto_copy(tbl(src_iris, "iris"), src_mtcars, name = "iris"),
    "`x` and `y` must share the same src, set `copy` = TRUE (may be slow)",
    fixed = TRUE
  )

  expect_error(
    auto_copy(tbl(src_mtcars, "mtcars"), src_iris, name = "mtcars"),
    "`x` and `y` must share the same src, set `copy` = TRUE (may be slow)",
    fixed = TRUE
  )
})

test_that("src_sqlite() errs if path does not exist", {
  skip_if_not_installed("dbplyr")

  expect_error(
    src_sqlite(":memory:"),
    "`path` must already exist, unless `create` = TRUE",
    fixed = TRUE
  )
})

test_that("src_tbls() includes all tbls (#4326)", {
  expect_equal(
    src_tbls(src_df(env = env(. = iris))),
    "."
  )
})
