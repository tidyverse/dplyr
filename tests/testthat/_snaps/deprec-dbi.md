# src_sqlite() gives meaningful error messages

    Code
      (expect_error(src_sqlite(":memory:")))
    Output
      <error/rlang_error>
      Error in `glubort()`: `path` must already exist, unless `create` = TRUE.

