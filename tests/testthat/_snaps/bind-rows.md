# bind_rows() give informative errors

    Code
      df1 <- tibble(x = 1:3)
      df2 <- tibble(x = 4:6)
      (expect_error(bind_rows(df1, df2, .id = 5)))
    Output
      <error/rlang_error>
      Error in `bind_rows()`:
      ! `.id` must be a single string, not a number.
    Code
      ll <- list(1:5, env(a = 1))
      (expect_error(bind_rows(ll)))
    Output
      <error/rlang_error>
      Error in `bind_rows()`:
      ! Argument 1 must have names.
    Code
      ll <- list(tibble(a = 1:5), env(a = 1))
      (expect_error(bind_rows(ll)))
    Output
      <error/rlang_error>
      Error in `bind_rows()`:
      ! Argument 2 must be a data frame or a named atomic vector.
    Code
      df1 <- tibble(a = factor("a"))
      df2 <- tibble(a = 1L)
      df3 <- tibble(a = 1)
      (expect_error(bind_rows(df1, df2)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `bind_rows()`:
      ! Can't combine `..1$a` <factor<4d52a>> and `..2$a` <integer>.
    Code
      (expect_error(bind_rows(df1, df3)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `bind_rows()`:
      ! Can't combine `..1$a` <factor<4d52a>> and `..2$a` <double>.
    Code
      df1 <- tibble(b = c(1, 2))
      df2 <- tibble(b = c(1L, 2L))
      df3 <- tibble(b = factor(c("A", "B")))
      df4 <- tibble(b = c("C", "D"))
      (expect_error(bind_rows(df1, df3)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `bind_rows()`:
      ! Can't combine `..1$b` <double> and `..2$b` <factor<a022a>>.
    Code
      (expect_error(bind_rows(df1, df4)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `bind_rows()`:
      ! Can't combine `..1$b` <double> and `..2$b` <character>.
    Code
      (expect_error(bind_rows(df2, df3)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `bind_rows()`:
      ! Can't combine `..1$b` <integer> and `..2$b` <factor<a022a>>.
    Code
      (expect_error(bind_rows(df2, df4)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `bind_rows()`:
      ! Can't combine `..1$b` <integer> and `..2$b` <character>.
    Code
      # # unnamed vectors
      (expect_error(bind_rows(1:2)))
    Output
      <error/rlang_error>
      Error in `bind_rows()`:
      ! Argument 1 must have names.

