# can't rename while partially `ungroup()`-ing (#6606)

    Code
      ungroup(gdf, g2 = g)
    Condition
      Error in `ungroup()`:
      ! Can't rename variables in this context.

# select(group_by(.)) implicitly adds grouping variables (#170)

    Code
      res <- select(group_by(mtcars, vs), mpg)
    Message
      Adding missing grouping variables: `vs`

# group_by works with zero-row data frames (#486)

    Code
      x <- select(dfg, a)
    Message
      Adding missing grouping variables: `g`

# group_by() and ungroup() give meaningful error messages

    Code
      df <- tibble(x = 1, y = 2)
      (expect_error(group_by(df, unknown)))
    Output
      <error/rlang_error>
      Error in `group_by()`:
      ! Must group by variables found in `.data`.
      x Column `unknown` is not found.
    Code
      (expect_error(ungroup(df, x)))
    Output
      <error/rlib_error_dots_nonempty>
      Error in `ungroup()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = x
      i Did you forget to name an argument?
    Code
      (expect_error(ungroup(group_by(df, x, y), z)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `ungroup()`:
      ! Can't select columns that don't exist.
      x Column `z` doesn't exist.
    Code
      (expect_error(group_by(df, z = a + 1)))
    Output
      <error/dplyr:::mutate_error>
      Error in `group_by()`:
      i In argument: `z = a + 1`.
      Caused by error:
      ! object 'a' not found

# group_by(add =) is defunct

    Code
      group_by(df, x, add = TRUE)
    Condition
      Error:
      ! The `add` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please use the `.add` argument instead.

# group_by_prepare(add =) is defunct

    Code
      group_by_prepare(df, x, add = TRUE)
    Condition
      Error:
      ! The `add` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please use the `.add` argument instead.

# group_by(.dots =) is defunct

    Code
      group_by(df, .dots = "x")
    Condition
      Error:
      ! The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.

# group_by_prepare(.dots =) is defunct

    Code
      group_by_prepare(df, .dots = "x")
    Condition
      Error:
      ! The `.dots` argument of `group_by()` was deprecated in dplyr 1.0.0 and is now defunct.

