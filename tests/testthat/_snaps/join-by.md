# has an informative print method

    Code
      join_by()
    Output
      Join By:
      - Cross

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
      join_by(a == a, preceding(b, c), following(d, e, inclusive = FALSE))
    Output
      Join By:
      - a == a
      - preceding(b, c)
      - following(d, e, inclusive = FALSE)

# has informative error messages

    Code
      join_by(a = b)
    Condition
      Error in `join_by()`:
      ! `join_by()` expressions can't be named.
      i Did you use `=` instead of `==`?

---

    Code
      join_by(NULL)
    Condition
      Error in `join_by()`:
      ! Join by expressions can't be empty.
      x Expression 1 is empty.

---

    Code
      join_by(foo(x > y))
    Condition
      Error in `join_by()`:
      ! Join by expressions must use one of: `==`, `>=`, `>`, `<=`, `<`, `preceding()`, `following()`, `between()`, `overlaps()`, or `within()`.
      i Expression 1 is `foo(x > y)`.

---

    Code
      join_by(x == y, x^y)
    Condition
      Error in `join_by()`:
      ! Join by expressions must use one of: `==`, `>=`, `>`, `<=`, `<`, `preceding()`, `following()`, `between()`, `overlaps()`, or `within()`.
      i Expression 2 is `x^y`.

---

    Code
      join_by(x + 1 == y)
    Condition
      Error in `join_by()`:
      ! `join_by()` expressions cannot contain computed columns, and can only reference columns by name or by explicitly specifying a side, like `x$col` or `y$col`.
      i Expression 1 contains `x + 1`.

---

    Code
      join_by(x == y + 1)
    Condition
      Error in `join_by()`:
      ! `join_by()` expressions cannot contain computed columns, and can only reference columns by name or by explicitly specifying a side, like `x$col` or `y$col`.
      i Expression 1 contains `y + 1`.

---

    Code
      join_by(1)
    Condition
      Error in `join_by()`:
      ! Each element of `...` must be a single column name or a join by expression.
      x Element 1 is not a name and not an expression.

---

    Code
      join_by(x$a)
    Condition
      Error in `join_by()`:
      ! When specifying a single column name, `$` cannot be used.
      i Expression 1 is `x$a`.

---

    Code
      join_by(z$a == y$b)
    Condition
      Error in `join_by()`:
      ! The left-hand side of a `$` expression must be either `x$` or `y$`.
      i Expression 1 contains `z$a`.

---

    Code
      join_by(x$a == z$b)
    Condition
      Error in `join_by()`:
      ! The left-hand side of a `$` expression must be either `x$` or `y$`.
      i Expression 1 contains `z$b`.

---

    Code
      join_by((x + 1)$y == b)
    Condition
      Error in `join_by()`:
      ! The left-hand side of a `$` expression must be a symbol or string.
      i Expression 1 contains `(x + 1)$y`.

---

    Code
      join_by(x$a == x$b)
    Condition
      Error in `join_by()`:
      ! The left and right-hand sides of a binary expression must reference different tables.
      i Expression 1 contains `x$a == x$b`.

---

    Code
      join_by(y$a == b)
    Condition
      Error in `join_by()`:
      ! The left and right-hand sides of a binary expression must reference different tables.
      i Expression 1 contains `y$a == b`.

---

    Code
      join_by(between(x$a, x$a, x$b))
    Condition
      Error in `join_by()`:
      ! Expressions containing `between()` can't all reference the same table.
      i Expression 1 is `between(x$a, x$a, x$b)`.

---

    Code
      join_by(within(x$a, x$b, x$a, x$b))
    Condition
      Error in `join_by()`:
      ! Expressions containing `overlaps()` or `within()` can't all reference the same table.
      i Expression 1 is `within(x$a, x$b, x$a, x$b)`.

---

    Code
      join_by(overlaps(a, b, x$a, x$b))
    Condition
      Error in `join_by()`:
      ! Expressions containing `overlaps()` or `within()` can't all reference the same table.
      i Expression 1 is `overlaps(a, b, x$a, x$b)`.

---

    Code
      join_by(between(a, x$a, y$b))
    Condition
      Error in `join_by()`:
      ! Expressions containing `between()` must reference the same table for the lower and upper bounds.
      i Expression 1 is `between(a, x$a, y$b)`.

---

    Code
      join_by(within(x$a, y$b, y$a, y$b))
    Condition
      Error in `join_by()`:
      ! Expressions containing `overlaps()` or `within()` must reference the same table for the left-hand side lower and upper bounds.
      i Expression 1 is `within(x$a, y$b, y$a, y$b)`.

---

    Code
      join_by(overlaps(x$a, x$b, y$a, x$b))
    Condition
      Error in `join_by()`:
      ! Expressions containing `overlaps()` or `within()` must reference the same table for the right-hand side lower and upper bounds.
      i Expression 1 is `overlaps(x$a, x$b, y$a, x$b)`.

---

    Code
      join_by(`>`(x))
    Condition
      Error:
      ! Expressions using `>` can't contain missing arguments.
      x Argument `y` is missing.

---

    Code
      join_by(between(x))
    Condition
      Error:
      ! Expressions using `between()` can't contain missing arguments.
      x Argument `y_lower` is missing.

---

    Code
      join_by(within(x))
    Condition
      Error:
      ! Expressions using `within()` can't contain missing arguments.
      x Argument `x_upper` is missing.

---

    Code
      join_by(overlaps(x))
    Condition
      Error:
      ! Expressions using `overlaps()` can't contain missing arguments.
      x Argument `x_upper` is missing.

---

    Code
      join_by(preceding(x))
    Condition
      Error:
      ! Expressions using `preceding()` can't contain missing arguments.
      x Argument `y` is missing.

---

    Code
      join_by(preceding(y = x))
    Condition
      Error:
      ! Expressions using `preceding()` can't contain missing arguments.
      x Argument `x` is missing.

---

    Code
      join_by(following(x))
    Condition
      Error:
      ! Expressions using `following()` can't contain missing arguments.
      x Argument `y` is missing.

---

    Code
      join_by(`$`(x) > y)
    Condition
      Error:
      ! Expressions using `$` can't contain missing arguments.
      x Argument `name` is missing.

---

    Code
      join_by(preceding(x, y, TRUE))
    Condition
      Error:
      ! `...` must be empty.
      i Non-empty dots were detected inside `preceding()`.

---

    Code
      join_by(following(x, y, TRUE))
    Condition
      Error:
      ! `...` must be empty.
      i Non-empty dots were detected inside `following()`.

---

    Code
      join_by(preceding(y$a, b))
    Condition
      Error in `join_by()`:
      ! The first argument to `preceding()` must reference the `x` table.
      i Expression 1 contains `preceding(y$a, b)`.

---

    Code
      join_by(preceding(a, x$b))
    Condition
      Error in `join_by()`:
      ! The second argument to `preceding()` must reference the `y` table.
      i Expression 1 contains `preceding(a, x$b)`.

# as_join_by() emits useful errors

    Code
      as_join_by(FALSE)
    Condition
      Error:
      ! `by` must be a (named) character vector, list, `join_by()` result, or NULL, not `FALSE`.

# join_by_common() emits useful information

    Code
      by <- join_by_common(c("x", "y"), c("x", "y"))
    Message
      Joining with `by = join_by(x, y)`

---

    Code
      by <- join_by_common(c("_x", "foo bar"), c("_x", "foo bar"))
    Message
      Joining with `by = join_by(`_x`, `foo bar`)`

---

    Code
      join_by_common(c("x", "y"), c("w", "z"))
    Condition
      Error:
      ! `by` must be supplied when `x` and `y` have no common variables.
      i Use `by = join_by()` to perform a cross-join.

