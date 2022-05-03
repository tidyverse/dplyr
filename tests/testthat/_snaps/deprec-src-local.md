# src_df() is deprecated / errors

    Code
      (expect_error(src_df("base", new.env())))
    Output
      <error/rlang_error>
      Error in `src_local()`:
      ! Exactly one of `pkg` and `env` must be non-NULL, not 2.
    Code
      (expect_error(src_df()))
    Output
      <error/rlang_error>
      Error in `src_local()`:
      ! Exactly one of `pkg` and `env` must be non-NULL, not 0.
    Code
      env <- new.env(parent = emptyenv())
      env$x <- 1
      src_env <- src_df(env = env)
      (expect_error(copy_to(src_env, tibble(x = 1), name = "x")))
    Output
      <error/rlang_error>
      Error in `copy_to()`:
      ! Object with `name` = `x` must not already exist, unless `overwrite` = TRUE.

