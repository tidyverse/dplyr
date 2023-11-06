# `relationship` default behavior is correct

    Code
      out <- join_rows(c(1, 1), c(1, 1), condition = "==")
    Condition
      Warning:
      Detected an unexpected many-to-many relationship between `x` and `y`.
      i Row 1 of `x` matches multiple rows in `y`.
      i Row 1 of `y` matches multiple rows in `x`.
      i If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.

# join_rows() allows `unmatched` to be specified independently for inner joins

    Code
      join_rows(c(1, 3), c(1, 2), type = "inner", unmatched = c("drop", "error"))
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

# join_rows() expects incompatible type errors to have been handled by join_cast_common()

    Code
      (expect_error(join_rows(data.frame(x = 1), data.frame(x = factor("a")))))
    Output
      <error/rlang_error>
      Error:
      ! `join_cast_common()` should have handled this.
      i This is an internal error that was detected in the dplyr package.
        Please report it at <https://github.com/tidyverse/dplyr/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

# join_rows() gives meaningful one-to-one errors

    Code
      join_rows(1, c(1, 1), relationship = "one-to-one")
    Condition
      Error:
      ! Each row in `x` must match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows in `y`.

---

    Code
      join_rows(c(1, 1), 1, relationship = "one-to-one")
    Condition
      Error:
      ! Each row in `y` must match at most 1 row in `x`.
      i Row 1 of `y` matches multiple rows in `x`.

# join_rows() gives meaningful one-to-many errors

    Code
      join_rows(c(1, 1), 1, relationship = "one-to-many")
    Condition
      Error:
      ! Each row in `y` must match at most 1 row in `x`.
      i Row 1 of `y` matches multiple rows in `x`.

# join_rows() gives meaningful many-to-one errors

    Code
      join_rows(1, c(1, 1), relationship = "many-to-one")
    Condition
      Error:
      ! Each row in `x` must match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows in `y`.

# join_rows() gives meaningful many-to-many warnings

    Code
      join_rows(c(1, 1), c(1, 1))
    Condition
      Warning:
      Detected an unexpected many-to-many relationship between `x` and `y`.
      i Row 1 of `x` matches multiple rows in `y`.
      i Row 1 of `y` matches multiple rows in `x`.
      i If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
    Output
      $x
      [1] 1 1 2 2
      
      $y
      [1] 1 2 1 2
      

---

    Code
      left_join(df, df, by = join_by(x))
    Condition
      Warning in `left_join()`:
      Detected an unexpected many-to-many relationship between `x` and `y`.
      i Row 1 of `x` matches multiple rows in `y`.
      i Row 1 of `y` matches multiple rows in `x`.
      i If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
    Output
        x
      1 1
      2 1
      3 1
      4 1

# join_rows() gives meaningful error message on unmatched rows

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), type = "left",
      unmatched = "error")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), type = "nest",
      unmatched = "error")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = c(3, 1)), type = "right",
      unmatched = "error")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = 1), type = "inner",
      unmatched = "error")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = c(1, 2)), data.frame(x = 1), type = "inner",
      unmatched = c("error", "drop"))
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, 2)), type = "inner",
      unmatched = "error")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, 2)), type = "inner",
      unmatched = c("drop", "error"))
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

# join_rows() always errors on unmatched missing values

    Code
      join_rows(data.frame(x = 1), data.frame(x = NA), type = "left", unmatched = "error",
      na_matches = "na")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "left", unmatched = "error",
      na_matches = "never")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = NA), type = "nest", unmatched = "error",
      na_matches = "na")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "nest", unmatched = "error",
      na_matches = "never")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 1 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = 1), type = "right", unmatched = "error",
      na_matches = "na")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 1 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "right", unmatched = "error",
      na_matches = "never")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 1 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, NA)), type = "inner",
      unmatched = "error", na_matches = "na")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = 1), data.frame(x = c(1, NA)), type = "inner",
      unmatched = c("drop", "error"), na_matches = "na")
    Condition
      Error:
      ! Each row of `y` must be matched by `x`.
      i Row 2 of `y` was not matched.

---

    Code
      join_rows(data.frame(x = c(1, NA)), data.frame(x = 1), type = "inner",
      unmatched = "error", na_matches = "na")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = c(1, NA)), data.frame(x = 1), type = "inner",
      unmatched = c("error", "drop"), na_matches = "na")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 2 of `x` does not have a match.

---

    Code
      join_rows(data.frame(x = NA), data.frame(x = NA), type = "inner", unmatched = "error",
      na_matches = "never")
    Condition
      Error:
      ! Each row of `x` must have a match in `y`.
      i Row 1 of `x` does not have a match.

# join_rows() validates `unmatched`

    Code
      join_rows(df, df, unmatched = 1)
    Condition
      Error:
      ! `unmatched` must be a character vector, not the number 1.
    Code
      join_rows(df, df, unmatched = "foo")
    Condition
      Error:
      ! `unmatched` must be one of "drop" or "error", not "foo".
    Code
      join_rows(df, df, type = "left", unmatched = character())
    Condition
      Error:
      ! `unmatched` must be length 1, not 0.
    Code
      join_rows(df, df, type = "left", unmatched = c("drop", "error"))
    Condition
      Error:
      ! `unmatched` must be length 1, not 2.
    Code
      join_rows(df, df, type = "inner", unmatched = character())
    Condition
      Error:
      ! `unmatched` must be length 1 or 2, not 0.
    Code
      join_rows(df, df, type = "inner", unmatched = c("drop", "error", "error"))
    Condition
      Error:
      ! `unmatched` must be length 1 or 2, not 3.
    Code
      join_rows(df, df, type = "inner", unmatched = c("drop", "dr"))
    Condition
      Error:
      ! `unmatched` must be one of "drop" or "error", not "dr".
      i Did you mean "drop"?

# join_rows() validates `relationship`

    Code
      join_rows(df, df, relationship = 1)
    Condition
      Error:
      ! `relationship` must be a string or character vector.

---

    Code
      join_rows(df, df, relationship = "none")
    Condition
      Error:
      ! `relationship` must be one of "one-to-one", "one-to-many", "many-to-one", or "many-to-many", not "none".

---

    Code
      join_rows(df, df, relationship = "warn-many-to-many")
    Condition
      Error:
      ! `relationship` must be one of "one-to-one", "one-to-many", "many-to-one", or "many-to-many", not "warn-many-to-many".
      i Did you mean "many-to-many"?

# join_rows() rethrows overflow error nicely (#6912)

    Code
      join_rows(df, df, condition = ">=")
    Condition
      Error:
      ! This join would result in more rows than dplyr can handle.
      i 50000005000000 rows would be returned. 2147483647 rows is the maximum number allowed.
      i Double check your join keys. This error commonly occurs due to a missing join key, or an improperly specified join condition.

# `multiple = NULL` is deprecated and results in `'all'` (#6731)

    Code
      out <- join_rows(df1, df2, multiple = NULL)
    Condition
      Warning:
      Specifying `multiple = NULL` was deprecated in dplyr 1.1.1.
      i Please use `multiple = "all"` instead.

---

    Code
      left_join(df1, df2, by = join_by(x), multiple = NULL)
    Condition
      Warning:
      Specifying `multiple = NULL` was deprecated in dplyr 1.1.1.
      i Please use `multiple = "all"` instead.
    Output
      # A tibble: 3 x 1
            x
        <dbl>
      1     1
      2     2
      3     2

# `multiple = 'error'` is deprecated (#6731)

    Code
      join_rows(df1, df2, multiple = "error")
    Condition
      Warning:
      Specifying `multiple = "error"` was deprecated in dplyr 1.1.1.
      i Please use `relationship = "many-to-one"` instead.
      Error:
      ! Each row in `x` must match at most 1 row in `y`.
      i Row 2 of `x` matches multiple rows in `y`.

---

    Code
      left_join(df1, df2, by = join_by(x), multiple = "error")
    Condition
      Warning:
      Specifying `multiple = "error"` was deprecated in dplyr 1.1.1.
      i Please use `relationship = "many-to-one"` instead.
      Error in `left_join()`:
      ! Each row in `x` must match at most 1 row in `y`.
      i Row 2 of `x` matches multiple rows in `y`.

# `multiple = 'warning'` is deprecated (#6731)

    Code
      out <- join_rows(df1, df2, multiple = "warning")
    Condition
      Warning:
      Specifying `multiple = "warning"` was deprecated in dplyr 1.1.1.
      i Please use `relationship = "many-to-one"` instead.
      Warning:
      Each row in `x` is expected to match at most 1 row in `y`.
      i Row 2 of `x` matches multiple rows.

---

    Code
      left_join(df1, df2, by = join_by(x), multiple = "warning")
    Condition
      Warning:
      Specifying `multiple = "warning"` was deprecated in dplyr 1.1.1.
      i Please use `relationship = "many-to-one"` instead.
      Warning in `left_join()`:
      Each row in `x` is expected to match at most 1 row in `y`.
      i Row 2 of `x` matches multiple rows.
    Output
      # A tibble: 3 x 1
            x
        <dbl>
      1     1
      2     2
      3     2

