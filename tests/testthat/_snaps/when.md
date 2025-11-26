# no recycling is performed!

    Code
      when_any(TRUE, c(TRUE, FALSE))
    Condition
      Error in `when_any()`:
      ! `..2` must have size 1, not size 2.

---

    Code
      when_all(TRUE, c(TRUE, FALSE))
    Condition
      Error in `when_all()`:
      ! `..2` must have size 1, not size 2.

---

    Code
      when_any(TRUE, size = 2)
    Condition
      Error in `when_any()`:
      ! `..1` must have size 2, not size 1.

---

    Code
      when_all(TRUE, size = 2)
    Condition
      Error in `when_all()`:
      ! `..1` must have size 2, not size 1.

# inputs must be strictly logical vectors

    Code
      when_any(1)
    Condition
      Error in `when_any()`:
      ! `..1` must be a logical vector, not the number 1.

---

    Code
      when_all(1)
    Condition
      Error in `when_all()`:
      ! `..1` must be a logical vector, not the number 1.

---

    Code
      when_any(array(TRUE))
    Condition
      Error in `when_any()`:
      ! `..1` must be a logical vector, not a logical 1D array.

---

    Code
      when_all(array(TRUE))
    Condition
      Error in `when_all()`:
      ! `..1` must be a logical vector, not a logical 1D array.

---

    Code
      when_any(structure(TRUE, class = "foo"))
    Condition
      Error in `when_any()`:
      ! `..1` must be a logical vector, not a <foo> object.

---

    Code
      when_all(structure(TRUE, class = "foo"))
    Condition
      Error in `when_all()`:
      ! `..1` must be a logical vector, not a <foo> object.

# `...` can't be named

    Code
      when_any(x = TRUE)
    Condition
      Error in `when_any()`:
      ! Arguments in `...` must be passed by position, not name.
      x Problematic argument:
      * x = TRUE

---

    Code
      when_all(x = TRUE)
    Condition
      Error in `when_all()`:
      ! Arguments in `...` must be passed by position, not name.
      x Problematic argument:
      * x = TRUE

# `na_rm` is validated

    Code
      when_any(na_rm = "x")
    Condition
      Error in `when_any()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "x".

---

    Code
      when_all(na_rm = "x")
    Condition
      Error in `when_all()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not the string "x".

# `size` is validated

    Code
      when_any(size = "x")
    Condition
      Error in `vec_pany()`:
      ! `.size` must be a scalar integer or double.

---

    Code
      when_all(size = "x")
    Condition
      Error in `vec_pall()`:
      ! `.size` must be a scalar integer or double.

