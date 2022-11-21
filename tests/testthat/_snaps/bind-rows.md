# bind_rows() only flattens S3 lists that inherit from list (#3924)

    Code
      bind_rows(lst1)
    Condition
      Error in `bind_rows()`:
      ! Argument 1 must be a data frame or a named atomic vector.

# bind_rows() validates lists (#5417)

    Code
      bind_rows(list(x = 1), list(x = 1:3, y = 1:2))
    Condition
      Error in `vctrs::data_frame()`:
      ! Can't recycle `x` (size 3) to match `y` (size 2).

# bind_rows() give informative errors

    Code
      # invalid .id
      df1 <- tibble(x = 1:3)
      df2 <- tibble(x = 4:6)
      (expect_error(bind_rows(df1, df2, .id = 5)))
    Output
      <error/rlang_error>
      Error in `bind_rows()`:
      ! `.id` must be a single string, not the number 5.
    Code
      # invalid type
      ll <- list(tibble(a = 1:5), env(a = 1))
      (expect_error(bind_rows(ll)))
    Output
      <error/rlang_error>
      Error in `bind_rows()`:
      ! Argument 2 must be a data frame or a named atomic vector.
    Code
      df1 <- tibble(a = factor("a"))
      df2 <- tibble(a = 1L)
      (expect_error(bind_rows(df1, df2)))
    Output
      <error/vctrs_error_ptype2>
      Error in `bind_rows()`:
      ! Can't combine `..1$a` <factor<4d52a>> and `..2$a` <integer>.
    Code
      # unnamed vectors
      (expect_error(bind_rows(1:2)))
    Output
      <error/rlang_error>
      Error in `bind_rows()`:
      ! Argument 1 must be a data frame or a named atomic vector.

