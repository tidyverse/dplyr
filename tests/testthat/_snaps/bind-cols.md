# bind_cols() repairs names

    Code
      bound <- bind_cols(df, df)
    Message
      New names:
      * `a` -> `a...1`
      * `b` -> `b...2`
      * `a` -> `a...3`
      * `b` -> `b...4`

# bind_cols() handles unnamed list with name repair (#3402)

    Code
      df <- bind_cols(list(1, 2))
    Message
      New names:
      * `` -> `...1`
      * `` -> `...2`

# bind_cols() gives informative errors

    Code
      # # incompatible size
      (expect_error(bind_cols(a = 1:2, mtcars)))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `bind_cols()`:
      ! Can't recycle `a` (size 2) to match `..2` (size 32).
    Code
      (expect_error(bind_cols(mtcars, a = 1:3)))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `bind_cols()`:
      ! Can't recycle `..1` (size 32) to match `a` (size 3).

