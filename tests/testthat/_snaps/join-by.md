# has an informative print method

    Code
      join_by()
    Output
      Join By:

---

    Code
      join_by(a, b)
    Output
      Join By:
      - a
      - b

---

    Code
      join_by("a", "b")
    Output
      Join By:
      - "a"
      - "b"

---

    Code
      join_by(a == a, b >= c)
    Output
      Join By:
      - a == a
      - b >= c

---

    Code
      join_by(a == a, b >= "c")
    Output
      Join By:
      - a == a
      - b >= "c"

---

    Code
      join_by(a == a, max(b >= "c"), min(d < e))
    Output
      Join By:
      - a == a
      - max(b >= "c")
      - min(d < e)

# has informative error messages

    Code
      join_by(a = b)
    Error <rlang_error>
      `join_by()` expressions can't be named.
      i Did you use `=` rather than `==`?

---

    Code
      join_by(NULL)
    Error <rlang_error>
      Join by expressions can't be empty.
      x Expression 1 is empty.

---

    Code
      join_by(foo(x > y))
    Error <rlang_error>
      `join_by()` expressions of length 2 must contain either `max()` or `min()`.
      x Expression 1 is `foo(x > y)`.

---

    Code
      join_by(max(!x))
    Error <rlang_error>
      Each `join_by()` condition must be length 1 or length 3, and be composed of a left-hand side column name, a condition, and a right-hand side column name.
      x Expression 1 is length 2 and is `!x`.

---

    Code
      join_by(x == y, x^y)
    Error <rlang_error>
      Each `join_by()` condition must be separated by one of: `==`, `>`, `>=`, `<`, or `<=`.
      x Expression 2 is `x^y`.

---

    Code
      join_by(x + 1 == y)
    Error <rlang_error>
      The left-hand side of each `join_by()` condition must be a string or an unquoted column name.
      i The left-hand side of condition 1 is `x + 1`.

---

    Code
      join_by(x == y + 1)
    Error <rlang_error>
      The right-hand side of each `join_by()` condition must be a string or an unquoted column name.
      i The right-hand side of condition 1 is `y + 1`.

# as_join_by() emits useful errors

    Code
      as_join_by(FALSE)
    Error <rlang_error>
      `by` must be a (named) character vector, list, `join_by()` result, or NULL, not a logical vector.

# join_by_common() emits useful information

    Code
      by <- join_by_common(c("x", "y"), c("x", "y"))
    Message <message>
      Joining, by = c("x", "y")

---

    Code
      join_by_common(c("x", "y"), c("w", "z"))
    Error <rlang_error>
      `by` must be supplied when `x` and `y` have no common variables.
      i use `by = character()` to perform a cross-join.

