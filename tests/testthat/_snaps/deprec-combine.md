# combine() is deprecated

    Code
      combine()
    Warning <lifecycle_warning_deprecated>
      `combine()` was deprecated in dplyr 1.0.0.
      Please use `vctrs::vec_c()` instead.
    Output
      logical(0)

# combine() gives meaningful error messages

    Code
      (expect_error(combine("a", 1)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`: Can't combine `..1` <character> and `..2` <double>.
    Code
      (expect_error(combine(factor("a"), 1L)))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_vctrs()`: Can't combine `..1` <factor<4d52a>> and `..2` <integer>.

