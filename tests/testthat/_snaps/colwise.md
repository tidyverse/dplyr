# colwise utils gives meaningful error messages

    Code
      (expect_error(tbl_at_vars(iris, raw(3))))
    Output
      <error/rlang_error>
      Error:
      ! `.vars` must be a character/numeric vector or a `vars()` object, not a raw vector.
    Code
      (expect_error(tbl_if_vars(iris, list(identity, force), environment())))
    Output
      <error/rlang_error>
      Error:
      ! `.predicate` must have length 1, not 2.
    Code
      .funs <- as_fun_list(list(identity, force), caller_env())
      (expect_error(tbl_if_vars(iris, .funs, environment())))
    Output
      <error/rlang_error>
      Error:
      ! `.predicate` must have length 1, not 2.

