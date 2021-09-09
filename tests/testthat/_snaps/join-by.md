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
      i Did you use `=` instead of `==`?

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
      Join by expressions must use one of: `==`, `>=`, `>`, `<=`, `<`, `between()`, `overlaps()`, `within()`, `max()`, or `min()`.
      x Expression 1 is `foo(x > y)`.

---

    Code
      join_by(max(!x))
    Error <rlang_error>
      `max()` or `min()` must wrap a binary condition.
      x Expression 1 is `max(!x)`.

---

    Code
      join_by(x == y, x^y)
    Error <rlang_error>
      Join by expressions must use one of: `==`, `>=`, `>`, `<=`, `<`, `between()`, `overlaps()`, `within()`, `max()`, or `min()`.
      x Expression 2 is `x^y`.

---

    Code
      join_by(x + 1 == y)
    Error <rlang_error>
      `join_by()` expressions cannot contain computed columns, and can only reference columns by name or by explicitly specifying a side, like `x$col` or `y$col`.
      x Expression 1 contains `x + 1`.

---

    Code
      join_by(x == y + 1)
    Error <rlang_error>
      `join_by()` expressions cannot contain computed columns, and can only reference columns by name or by explicitly specifying a side, like `x$col` or `y$col`.
      x Expression 1 contains `y + 1`.

---

    Code
      join_by(1)
    Error <rlang_error>
      Each element of `...` must be a single column name or a join by expression.
      x Element 1 is not a name and not an expression.

---

    Code
      join_by(x$a)
    Error <rlang_error>
      When specifying a single column name, `$` cannot be used.
      x Expression 1 is `x$a`.

---

    Code
      join_by(z$a == y$b)
    Error <rlang_error>
      The left-hand side of a `$` expression must be either `x$` or `y$`.
      x Expression 1 contains `z$a`.

---

    Code
      join_by(x$a == z$b)
    Error <rlang_error>
      The left-hand side of a `$` expression must be either `x$` or `y$`.
      x Expression 1 contains `z$b`.

---

    Code
      join_by((x + 1)$y == b)
    Error <rlang_error>
      The left-hand side of a `$` expression must be a symbol or string.
      x Expression 1 contains `(x + 1)$y`.

---

    Code
      join_by(x$a == x$b)
    Error <rlang_error>
      The left and right-hand sides of a binary expression must reference different tables.
      x Expression 1 contains `x$a == x$b`.

---

    Code
      join_by(y$a == b)
    Error <rlang_error>
      The left and right-hand sides of a binary expression must reference different tables.
      x Expression 1 contains `y$a == b`.

---

    Code
      join_by(between(x$a, x$a, x$b))
    Error <rlang_error>
      Expressions containing `between()` can't all reference the same table.
      x Expression 1 is `between(x$a, x$a, x$b)`.

---

    Code
      join_by(within(x$a, x$b, x$a, x$b))
    Error <rlang_error>
      Expressions containing `overlaps()` or `within()` can't all reference the same table.
      x Expression 1 is `within(x$a, x$b, x$a, x$b)`.

---

    Code
      join_by(overlaps(a, b, x$a, x$b))
    Error <rlang_error>
      Expressions containing `overlaps()` or `within()` can't all reference the same table.
      x Expression 1 is `overlaps(a, b, x$a, x$b)`.

---

    Code
      join_by(between(a, x$a, y$b))
    Error <rlang_error>
      Expressions containing `between()` must reference the same table for the lower and upper bounds.
      x Expression 1 is `between(a, x$a, y$b)`.

---

    Code
      join_by(within(x$a, y$b, y$a, y$b))
    Error <rlang_error>
      Expressions containing `overlaps()` or `within()` must reference the same table for the left-hand side lower and upper bounds.
      x Expression 1 is `within(x$a, y$b, y$a, y$b)`.

---

    Code
      join_by(overlaps(x$a, x$b, y$a, x$b))
    Error <rlang_error>
      Expressions containing `overlaps()` or `within()` must reference the same table for the right-hand side lower and upper bounds.
      x Expression 1 is `overlaps(x$a, x$b, y$a, x$b)`.

---

    Code
      join_by(between(x))
    Error <rlang_error>
      Expressions containing `between()` must have 3 arguments.
      x Expression 1 is `between(x)`, which has 1 argument(s).

---

    Code
      join_by(within(x))
    Error <rlang_error>
      Expressions containing `overlaps()` or `within()` must have 4 arguments.
      x Expression 1 is `within(x)`, which has 1 argument(s).

---

    Code
      join_by(overlaps(x))
    Error <rlang_error>
      Expressions containing `overlaps()` or `within()` must have 4 arguments.
      x Expression 1 is `overlaps(x)`, which has 1 argument(s).

---

    Code
      join_by(between(x = x, y, z))
    Error <rlang_error>
      The arguments of `between()` must not be named.
      x Expression 1 is `between(x = x, y, z)`.

---

    Code
      join_by(within(x = x, y, z, w))
    Error <rlang_error>
      The arguments of `overlaps()` and `within()` must not be named.
      x Expression 1 is `within(x = x, y, z, w)`.

---

    Code
      join_by(overlaps(x = x, y, z, w))
    Error <rlang_error>
      The arguments of `overlaps()` and `within()` must not be named.
      x Expression 1 is `overlaps(x = x, y, z, w)`.

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

