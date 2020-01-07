context("copy_to")

test_that("src_local only overwrites if overwrite = TRUE", {
  env <- new.env(parent = emptyenv())
  env$x <- 1

  src_env <- src_df(env = env)

  df <- tibble(x = 1)
  copy_to(src_env, df, name = "x", overwrite = TRUE)
  expect_equal(env$x, df)
})

test_that("src_tbls() includes all tbls (#4326)", {
  expect_equal(
    src_tbls(src_df(env = env(. = iris))),
    "."
  )
})

# Errors --------------------------------------------

test_that("copy_to() gives meaningful error messages", {
  skip_if_not_installed("dbplyr")

  verify_output(test_path("test-copy_to-errors.txt"), {
    "# path does not exist"
    src_sqlite(":memory:")

    "# requires same source"
    env <- new.env(parent = emptyenv())
    env$iris <- iris
    src_iris <- src_df(env = env)

    src_mtcars <- src_sqlite(":memory:", create = TRUE)
    copy_to(src_mtcars, mtcars, "mtcars")

    auto_copy(tbl(src_iris, "iris"), src_mtcars, name = "iris")
    auto_copy(tbl(src_mtcars, "mtcars"), src_iris, name = "mtcars")

    "# src_local errs with pkg/env"
    src_df("base", new.env())
    src_df()

    "# only overwrite if overwrite = TRUE"
    env <- new.env(parent = emptyenv())
    env$x <- 1

    src_env <- src_df(env = env)
    copy_to(src_env, tibble(x = 1), name = "x")
  })
})
