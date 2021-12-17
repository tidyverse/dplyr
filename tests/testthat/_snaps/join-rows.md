# join_rows() gives meaningful error message on incompatible types

    Code
      (expect_error(join_rows(data.frame(x = 1), data.frame(x = factor("a")))))
    Output
      <error/rlang_error>
      Error:
      ! Can't join on `x$x` x `y$x` because of incompatible types.
      i `x$x` is of type <double>>.
      i `y$x` is of type <factor<4d52a>>>.

