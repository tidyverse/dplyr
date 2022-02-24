# rows_insert() doesn't allow insertion of duplicate keys

    Code
      (expect_error(rows_insert(x, y, by = "a")))
    Output
      <error/rlang_error>
      Error in `rows_insert()`:
      ! Can't insert rows with keys that already exist in `x`.
      i The following rows in `y` have keys that already exist in `x`: 1.

---

    Code
      (expect_error(rows_insert(x, y, by = "a")))
    Output
      <error/rlang_error>
      Error in `rows_insert()`:
      ! Can't insert rows with keys that already exist in `x`.
      i The following rows in `y` have keys that already exist in `x`: 1, 2, and 3.

# rows_delete()

    Code
      res <- rows_delete(data, tibble(a = 2:3, b = "b"), by = "a")
    Message
      Ignoring extra columns: b

# rows_check_containment() checks that `y` columns are in `x`

    Code
      (expect_error(rows_check_containment(x, y)))
    Output
      <error/rlang_error>
      Error:
      ! All columns in `y` must exist in `x`.
      i The following columns only exist in `y`: `b`.

# rows_check_by() checks that `y` has at least 1 column before using it (#6061)

    Code
      (expect_error(rows_check_by(by = NULL, y = y)))
    Output
      <error/rlang_error>
      Error in `rows_check_by()`:
      ! `y` must have at least one column to use as a key.

# rows_check_by() uses the first column from `y` by default, with a message

    Code
      by <- rows_check_by(by = NULL, y = y)
    Message
      Matching, by = "a"

# rows_check_by() validates `by`

    Code
      (expect_error(rows_check_by(by = 1, y = y)))
    Output
      <error/rlang_error>
      Error:
      ! `by` must be a character vector.
    Code
      (expect_error(rows_check_by(by = character(), y = y)))
    Output
      <error/rlang_error>
      Error:
      ! Must specify at least 1 column in `by`.
    Code
      (expect_error(rows_check_by(by = c(x = "y"), y = y)))
    Output
      <error/rlang_error>
      Error:
      ! `by` must be unnamed.

# rows_select_key() checks that all `by` columns are in `x`

    Code
      (expect_error(rows_select_key(x, "y", arg = "x")))
    Output
      <error/rlang_error>
      Error:
      ! All `by` columns must exist in `x`.
      i The following columns are missing from `x`: `y`.
    Code
      (expect_error(rows_select_key(x, c("y", "x", "z"), arg = "y")))
    Output
      <error/rlang_error>
      Error:
      ! All `by` columns must exist in `y`.
      i The following columns are missing from `y`: `y` and `z`.

# rows_select_key() optionally requires uniqueness

    Code
      (expect_error(rows_select_key(x, "x", arg = "x", unique = TRUE)))
    Output
      <error/rlang_error>
      Error:
      ! `x` key values must be unique.
    Code
      (expect_error(rows_select_key(x, c("x", "y"), arg = "y", unique = TRUE)))
    Output
      <error/rlang_error>
      Error:
      ! `y` key values must be unique.

# rows_*() errors

    Code
      data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)
      (expect_error(rows_update(data, tibble(a = 2:3, b = "z"), by = c("a", "b"))))
    Output
      <error/rlang_error>
      Error in `rows_update()`:
      ! Attempting to update missing rows.
    Code
      (expect_error(rows_patch(data, tibble(a = 2:3, b = "z"), by = c("a", "b"))))
    Output
      <error/rlang_error>
      Error in `rows_patch()`:
      ! Can't patch missing row.
    Code
      (expect_error(rows_delete(data, tibble(a = 2:4))))
    Message
      Matching, by = "a"
    Output
      <error/rlang_error>
      Error in `rows_delete()`:
      ! Can't delete missing row.
    Code
      (expect_error(rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b"))))
    Output
      <error/rlang_error>
      Error in `rows_delete()`:
      ! Can't delete missing row.
    Code
      rows_delete(data, tibble(a = 2:3))
    Message
      Matching, by = "a"
    Output
      # A tibble: 1 x 3
            a b         c
        <int> <chr> <dbl>
      1     1 a       0.5
    Code
      rows_delete(data, tibble(a = 2:3, b = "b"))
    Message
      Matching, by = "a"
      Ignoring extra columns: b
    Output
      # A tibble: 1 x 3
            a b         c
        <int> <chr> <dbl>
      1     1 a       0.5

