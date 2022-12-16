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
      <error/vctrs_error_ptype2>
      Error in `coalesce()`:
      ! Can't combine `..1` <integer> and `..2` <character>.

# `.size` overrides the common size

    Code
      coalesce(x, 1:2, .size = vec_size(x))
    Condition
      Error in `coalesce()`:
      ! Can't recycle `..2` (size 2) to size 1.

# must have at least one non-`NULL` vector

    Code
      coalesce()
    Condition
      Error in `coalesce()`:
      ! `...` can't be empty.

---

    Code
      coalesce(NULL, NULL)
    Condition
      Error in `coalesce()`:
      ! `...` can't be empty.

# inputs must be vectors

    Code
      coalesce(1, environment())
    Condition
      Error in `coalesce()`:
      ! `..2` must be a vector, not an environment.

# names in error messages are indexed correctly

    Code
      coalesce(1, "x")
    Condition
      Error in `coalesce()`:
      ! Can't combine `..1` <double> and `..2` <character>.

---

    Code
      coalesce(1, y = "x")
    Condition
      Error in `coalesce()`:
      ! Can't combine `..1` <double> and `y` <character>.

