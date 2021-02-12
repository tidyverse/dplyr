# coalesce() gives meaningful error messages

    Code
      coalesce(1:2, 1:3)
    Error <vctrs_error_incompatible_size>
      Can't recycle `..1` (size 2) to match `..2` (size 3).

---

    Code
      coalesce(1:2, letters[1:2])
    Error <vctrs_error_incompatible_type>
      Can't combine `..1` <integer> and `..2` <character>.

