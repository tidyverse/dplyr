context("copy_to")

test_that("src_local only overwrites if overwrite = TRUE", {
  env <- new.env(parent = emptyenv())
  env$x <- 1

  src_env <- src_df(env = env)

  expect_error(
    copy_to(src_env, tibble(x = 1), name = "x"),
    "`name`, `overwrite`: object with name `x` already exists, set `overwrite` = TRUE",
    fixed = TRUE
  )

  df <- tibble(x = 1)
  copy_to(src_env, df, name = "x", overwrite = TRUE)
  expect_equal(env$x, df)
})

test_that("src_local errs with pkg/env", {
  expect_error(
    src_df("base", new.env()),
    "`pkg`, `env`: exactly one must be non-NULL, not 2",
    fixed = TRUE
  )

  expect_error(
    src_df(),
    "`pkg`, `env`: exactly one must be non-NULL, not 0",
    fixed = TRUE
  )
})

test_that("auto_copy() requires same source", {
  requireNamespace("dbplyr")

  env <- new.env(parent = emptyenv())
  env$iris <- iris
  src_iris <- src_df(env = env)

  src_mtcars <- src_sqlite(":memory:", create = TRUE)
  copy_to(src_mtcars, mtcars, "mtcars")

  expect_error(
    auto_copy(tbl(src_iris, "iris"), src_mtcars, name = "iris"),
    "`x`, `y`, `copy`: must share the same src, set `copy` = TRUE (may be slow)",
    fixed = TRUE
  )

  expect_error(
    auto_copy(tbl(src_mtcars, "mtcars"), src_iris, name = "mtcars"),
    "`x`, `y`, `copy`: must share the same src, pass TRUE to copy (may be slow)",
    fixed = TRUE
  )
})

test_that("src_sqlite() errs if path does not exist", {
  expect_error(
    src_sqlite(":memory:"),
    "`path`, `create`: path does not exist, set `create` = TRUE",
    fixed = TRUE
  )
})

