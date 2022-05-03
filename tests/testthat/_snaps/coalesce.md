# coalesce() gives meaningful error messages

    Code
      (expect_error(coalesce(1:2, 1:3)))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `coalesce()`:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).
    Code
      (expect_error(coalesce(1:2, letters[1:2])))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `coalesce()`:
      ! Can't combine `..1` <integer> and `..2` <character>.

