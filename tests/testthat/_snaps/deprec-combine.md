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
      combine("a", 1)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1` <character> and `..2` <double>.

---

    Code
      combine(factor("a"), 1L)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1` <factor<127a2>> and `..2` <integer>.

