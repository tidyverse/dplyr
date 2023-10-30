# can't set attributes with `NULL`

    Code
      dplyr_set_attributes(1, NULL)
    Condition
      Error:
      ! Internal error: `attributes` must be a list.

# can't set attributes on `NULL`

    Code
      dplyr_set_attributes(NULL, list(x = 1))
    Condition
      Error:
      ! Internal error: `x` can't be `NULL`.

# can't set attributes with unnamed elements

    Code
      dplyr_set_attributes(1, list())
    Condition
      Error:
      ! Internal error: `attributes` must be named.

---

    Code
      dplyr_set_attributes(1, list(x = 1, 2))
    Condition
      Error:
      ! All attributes must have names. Attribute 2 does not.

