# rows_insert() doesn't allow insertion of matched keys by default

    Code
      (expect_error(rows_insert(x, y, by = "a")))
    Output
      <error/rlang_error>
      Error in `rows_insert()`:
      ! `y` can't contain keys that already exist in `x`.
      i The following rows in `y` have keys that already exist in `x`: `c(1)`.
      i Use `conflict = "ignore"` if you want to ignore these `y` rows.

---

    Code
      (expect_error(rows_insert(x, y, by = "a")))
    Output
      <error/rlang_error>
      Error in `rows_insert()`:
      ! `y` can't contain keys that already exist in `x`.
      i The following rows in `y` have keys that already exist in `x`: `c(1, 2, 3)`.
      i Use `conflict = "ignore"` if you want to ignore these `y` rows.

# `conflict` is validated

    Code
      (expect_error(rows_insert(x, y, by = "a", conflict = "foo")))
    Output
      <error/rlang_error>
      Error in `rows_insert()`:
      ! `conflict` must be one of "error" or "ignore", not "foo".
    Code
      (expect_error(rows_insert(x, y, by = "a", conflict = 1)))
    Output
      <error/rlang_error>
      Error in `rows_insert()`:
      ! `conflict` must be a character vector, not a number.

# rows_update() requires `y` keys to exist in `x` by default

    Code
      (expect_error(rows_update(x, y, "a")))
    Output
      <error/rlang_error>
      Error in `rows_update()`:
      ! `y` must contain keys that already exist in `x`.
      i The following rows in `y` have keys that don't exist in `x`: `c(1, 3)`.
      i Use `unmatched = "ignore"` if you want to ignore these `y` rows.

# rows_update() doesn't allow `y` keys to be duplicated (#5553)

    Code
      (expect_error(rows_update(x, y, by = "a")))
    Output
      <error/rlang_error>
      Error in `rows_update()`:
      ! `y` key values must be unique.
      i The following rows contain duplicate key values: `c(1, 2)`.

# `unmatched` is validated

    Code
      (expect_error(rows_update(x, y, by = "a", unmatched = "foo")))
    Output
      <error/rlang_error>
      Error in `rows_update()`:
      ! `unmatched` must be one of "error" or "ignore", not "foo".
    Code
      (expect_error(rows_update(x, y, by = "a", unmatched = 1)))
    Output
      <error/rlang_error>
      Error in `rows_update()`:
      ! `unmatched` must be a character vector, not a number.

# rows_patch() requires `y` keys to exist in `x` by default

    Code
      (expect_error(rows_patch(x, y, "a")))
    Output
      <error/rlang_error>
      Error in `rows_patch()`:
      ! `y` must contain keys that already exist in `x`.
      i The following rows in `y` have keys that don't exist in `x`: `c(1, 3)`.
      i Use `unmatched = "ignore"` if you want to ignore these `y` rows.

# rows_patch() doesn't allow `y` keys to be duplicated (#5553)

    Code
      (expect_error(rows_patch(x, y, by = "a")))
    Output
      <error/rlang_error>
      Error in `rows_patch()`:
      ! `y` key values must be unique.
      i The following rows contain duplicate key values: `c(1, 2)`.

# rows_upsert() doesn't allow `y` keys to be duplicated (#5553)

    Code
      (expect_error(rows_upsert(x, y, by = "a")))
    Output
      <error/rlang_error>
      Error in `rows_upsert()`:
      ! `y` key values must be unique.
      i The following rows contain duplicate key values: `c(1, 2)`.

# rows_delete() ignores extra `y` columns, with a message

    Code
      out <- rows_delete(x, y)
    Message
      Matching, by = "a"
      Ignoring extra `y` columns: b

---

    Code
      out <- rows_delete(x, y, by = "a")
    Message
      Ignoring extra `y` columns: b

# rows_delete() requires `y` keys to exist in `x` by default

    Code
      (expect_error(rows_delete(x, y, "a")))
    Output
      <error/rlang_error>
      Error in `rows_delete()`:
      ! `y` must contain keys that already exist in `x`.
      i The following rows in `y` have keys that don't exist in `x`: `c(1, 3)`.
      i Use `unmatched = "ignore"` if you want to ignore these `y` rows.

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
      Error:
      ! `y` must have at least one column.

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
      ! `by` must specify at least 1 column.
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
      ! All columns specified through `by` must exist in `x` and `y`.
      i The following columns are missing from `x`: `y`.
    Code
      (expect_error(rows_select_key(x, c("y", "x", "z"), arg = "y")))
    Output
      <error/rlang_error>
      Error:
      ! All columns specified through `by` must exist in `x` and `y`.
      i The following columns are missing from `y`: `y` and `z`.

# rows_select_key() optionally requires uniqueness

    Code
      (expect_error(rows_select_key(x, "x", arg = "x", unique = TRUE)))
    Output
      <error/rlang_error>
      Error:
      ! `x` key values must be unique.
      i The following rows contain duplicate key values: `c(1, 2, 3)`.
    Code
      (expect_error(rows_select_key(x, c("x", "y"), arg = "y", unique = TRUE)))
    Output
      <error/rlang_error>
      Error:
      ! `y` key values must be unique.
      i The following rows contain duplicate key values: `c(1, 3)`.

