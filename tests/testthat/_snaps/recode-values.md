# when `from` is a list, `to` must recycle to the same size as that list

    Code
      recode_values(1, from = list(1, 2, 3), to = c(1, 2))
    Condition
      Error in `recode_values()`:
      ! Can't recycle `to` (size 2) to size 3.

# `NA` is considered unmatched unless handled explicitly

    Code
      recode_values(x, from = table$from, to = table$to, unmatched = "error")
    Condition
      Error in `recode_values()`:
      ! Each location must be matched.
      x Location 4 is unmatched.

# `x` must be a vector

    Code
      recode_values(x, 1 ~ 1)
    Condition
      Error in `recode_values()`:
      ! `x` must be a vector, not a <lm> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

---

    Code
      replace_values(x, 1 ~ 1)
    Condition
      Error in `replace_values()`:
      ! `x` must be a vector, not a <lm> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

# respects `ptype`

    Code
      recode_values(1, 1 ~ 0L, ptype = character())
    Condition
      Error in `recode_values()`:
      ! Can't convert `..1 (right)` <integer> to <character>.

---

    Code
      recode_values(1, 1 ~ "x", NULL, 2 ~ 0L, ptype = character())
    Condition
      Error in `recode_values()`:
      ! Can't convert `..3 (right)` <integer> to <character>.

---

    Code
      recode_values(1, from = 1, to = 0L, ptype = character())
    Condition
      Error in `recode_values()`:
      ! Can't convert `to` <integer> to <character>.

---

    Code
      recode_values(1, from = 1, to = "x", default = 0L, ptype = character())
    Condition
      Error in `recode_values()`:
      ! Can't convert `default` <integer> to <character>.

# `replace_values()` is type stable on `x`

    Code
      replace_values(x, "c" ~ "b")
    Condition
      Error in `replace_values()`:
      ! Can't convert from `..1 (left)` <character> to <factor<38051>> due to loss of generality.
      * Locations: 1

---

    Code
      replace_values(x, from = "c", to = "b")
    Condition
      Error in `replace_values()`:
      ! Can't convert from `from` <character> to `x` <factor<38051>> due to loss of generality.
      * Locations: 1

---

    Code
      replace_values(x, "a" ~ "c")
    Condition
      Error in `replace_values()`:
      ! Can't convert from `..1 (right)` <character> to <factor<38051>> due to loss of generality.
      * Locations: 1

---

    Code
      replace_values(x, from = "a", to = "c")
    Condition
      Error in `replace_values()`:
      ! Can't convert from `to` <character> to <factor<38051>> due to loss of generality.
      * Locations: 1

---

    Code
      replace_values(x, "a" ~ "b", NULL, "b" ~ "c")
    Condition
      Error in `replace_values()`:
      ! Can't convert from `..3 (right)` <character> to <factor<38051>> due to loss of generality.
      * Locations: 1

# `default` is part of `ptype` determination

    Code
      recode_values(1, from = 1, to = 0L, default = "x")
    Condition
      Error in `recode_values()`:
      ! Can't combine `to` <integer> and `default` <character>.

# `default` has its size checked

    Code
      recode_values(1:3, 1 ~ 0, default = 1:5)
    Condition
      Error in `recode_values()`:
      ! Can't recycle `default` (size 5) to size 3.

# treats list `from` and `to` as lists of vectors

    Code
      recode_values(x, from = from, to = to)
    Condition
      Error in `recode_values()`:
      ! Can't convert `from[[1]]` <integer> to <list>.

# `...` must be unnamed

    Code
      recode_values(1, foo = 1 ~ 2)
    Condition
      Error in `recode_values()`:
      ! Arguments in `...` must be passed by position, not name.
      x Problematic argument:
      * foo = 1 ~ 2

---

    Code
      replace_values(1, foo = 1 ~ 2)
    Condition
      Error in `replace_values()`:
      ! Arguments in `...` must be passed by position, not name.
      x Problematic argument:
      * foo = 1 ~ 2

# `...` must contain two sided formulas

    Code
      recode_values(1, 1 ~ 1, 2)
    Condition
      Error in `recode_values()`:
      ! Case 2 (`2`) must be a two-sided formula, not the number 2.

---

    Code
      replace_values(1, 1 ~ 1, 2)
    Condition
      Error in `replace_values()`:
      ! Case 2 (`2`) must be a two-sided formula, not the number 2.

---

    Code
      recode_values(1, 1 ~ 1, ~2)
    Condition
      Error in `recode_values()`:
      ! Case 2 (`~2`) must be a two-sided formula, not a one-sided formula.

---

    Code
      replace_values(1, 1 ~ 1, ~2)
    Condition
      Error in `replace_values()`:
      ! Case 2 (`~2`) must be a two-sided formula, not a one-sided formula.

# throws correct errors based on all combinations of `...` and `from` and `to`

    Code
      recode_values(1)
    Condition
      Error in `recode_values()`:
      ! `...` can't be empty.

---

    Code
      recode_values(1, 1 ~ 2, from = 1)
    Condition
      Error in `recode_values()`:
      ! Can't supply both `from` and `...`.

---

    Code
      replace_values(1, 1 ~ 2, from = 1)
    Condition
      Error in `replace_values()`:
      ! Can't supply both `from` and `...`.

---

    Code
      recode_values(1, from = 1)
    Condition
      Error in `recode_values()`:
      ! Must supply both `from` and `to`.

---

    Code
      replace_values(1, from = 1)
    Condition
      Error in `replace_values()`:
      ! Must supply both `from` and `to`.

---

    Code
      recode_values(1, to = 1)
    Condition
      Error in `recode_values()`:
      ! Must supply both `from` and `to`.

---

    Code
      replace_values(1, to = 1)
    Condition
      Error in `replace_values()`:
      ! Must supply both `from` and `to`.

