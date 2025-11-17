# coalesce() gives meaningful error messages

    Code
      coalesce(1:2, 1:3)
    Condition
      Error in `coalesce()`:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).

---

    Code
      coalesce(1:2, letters[1:2])
    Condition
      Error in `coalesce()`:
      ! Can't combine `..1` <integer> and `..2` <character>.

# `.size` overrides the common size

    Code
      coalesce(x, 1:2, .size = vec_size(x))
    Condition
      Error in `coalesce()`:
      ! Can't recycle `..2` (size 2) to size 1.

# can't be empty

    Code
      coalesce()
    Condition
      Error in `coalesce()`:
      ! `...` can't be empty.

# must have at least one non-`NULL` vector

    Code
      coalesce(NULL, NULL)
    Condition
      Error in `coalesce()`:
      ! `...` must contain at least 1 non-`NULL` value.

# inputs must be vectors

    Code
      coalesce(1, environment())
    Condition
      Error in `coalesce()`:
      ! `..2` must be a vector, not an environment.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.

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

---

    Code
      coalesce(1:2, 1:3)
    Condition
      Error in `coalesce()`:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).

---

    Code
      coalesce(1:2, y = 1:3)
    Condition
      Error in `coalesce()`:
      ! Can't recycle `..1` (size 2) to match `y` (size 3).

---

    Code
      coalesce(1, NULL, "x")
    Condition
      Error in `coalesce()`:
      ! Can't combine `..1` <double> and `..3` <character>.

---

    Code
      coalesce(1, NULL, y = "x")
    Condition
      Error in `coalesce()`:
      ! Can't combine `..1` <double> and `y` <character>.

---

    Code
      coalesce(1:2, NULL, 1:3)
    Condition
      Error in `coalesce()`:
      ! Can't recycle `..1` (size 2) to match `..3` (size 3).

---

    Code
      coalesce(1:2, NULL, y = 1:3)
    Condition
      Error in `coalesce()`:
      ! Can't recycle `..1` (size 2) to match `y` (size 3).

