# joining by nothing is an error

    Code
      join_by()
    Condition
      Error in `join_by()`:
      ! Must supply at least one expression.
      i If you want a cross join, use `cross_join()`.

# nicely catches required missing arguments when wrapped

    Code
      fn(a)
    Condition
      Error:
      ! Expressions using `==` can't contain missing arguments.
      x Argument `y` is missing.

# allows for namespaced helpers (#6838)

    Code
      join_by(dplyr::between(x, left, right))
    Output
      Join By:
      - dplyr::between(x, left, right)

---

    Code
      join_by(dplyr::within(xl, xu, yl, yu))
    Output
      Join By:
      - dplyr::within(xl, xu, yl, yu)

---

    Code
      join_by(dplyr::overlaps(xl, xu, yl, yu))
    Output
      Join By:
      - dplyr::overlaps(xl, xu, yl, yu)

---

    Code
      join_by(dplyr::closest(x < y))
    Output
      Join By:
      - dplyr::closest(x < y)

# has an informative print method

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
      join_by(a == a, closest(b >= c), closest(d < e))
    Output
      Join By:
      - a == a
      - closest(b >= c)
      - closest(d < e)

# has informative error messages

    Code
      join_by(a = b)
    Condition
      Error in `join_by()`:
      ! Can't name join expressions.
      i Did you use `=` instead of `==`?

---

    Code
      join_by(NULL)
    Condition
      Error in `join_by()`:
      ! Expressions can't be empty.
      x Expression 1 is empty.

---

    Code
      join_by(foo(x > y))
    Condition
      Error in `join_by()`:
      ! Expressions must use one of: `==`, `>=`, `>`, `<=`, `<`, `closest()`, `between()`, `overlaps()`, or `within()`.
      i Expression 1 is `foo(x > y)`.

---

    Code
      join_by(x == y, x^y)
    Condition
      Error in `join_by()`:
      ! Expressions must use one of: `==`, `>=`, `>`, `<=`, `<`, `closest()`, `between()`, `overlaps()`, or `within()`.
      i Expression 2 is `x^y`.

---

    Code
      join_by(x + 1 == y)
    Condition
      Error in `join_by()`:
      ! Expressions can't contain computed columns, and can only reference columns by name or by explicitly specifying a side, like `x$col` or `y$col`.
      i Expression 1 contains `x + 1`.

---

    Code
      join_by(x == y + 1)
    Condition
      Error in `join_by()`:
      ! Expressions can't contain computed columns, and can only reference columns by name or by explicitly specifying a side, like `x$col` or `y$col`.
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
      join_by(1())
    Condition
      Error in `join_by()`:
      ! Expressions must use one of: `==`, `>=`, `>`, `<=`, `<`, `closest()`, `between()`, `overlaps()`, or `within()`.
      i Expression 1 is `1()`.

---

    Code
      join_by(dplyrr::between(x, left, right))
    Condition
      Error in `join_by()`:
      ! Expressions can only be namespace prefixed with `dplyr::`.
      i Expression 1 is `dplyrr::between(x, left, right)`.

---

    Code
      join_by(x$a)
    Condition
      Error in `join_by()`:
      ! Can't use `$` when specifying a single column name.
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
      ! Expressions containing `within()` can't all reference the same table.
      i Expression 1 is `within(x$a, x$b, x$a, x$b)`.

---

    Code
      join_by(overlaps(a, b, x$a, x$b))
    Condition
      Error in `join_by()`:
      ! Expressions containing `overlaps()` can't all reference the same table.
      i Expression 1 is `overlaps(a, b, x$a, x$b)`.

---

    Code
      join_by(closest(x$a >= x$b))
    Condition
      Error in `join_by()`:
      ! The left and right-hand sides of a binary expression must reference different tables.
      i Expression 1 contains `x$a >= x$b`.

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
      ! Expressions containing `within()` must reference the same table for the left-hand side lower and upper bounds.
      i Expression 1 is `within(x$a, y$b, y$a, y$b)`.

---

    Code
      join_by(overlaps(x$a, x$b, y$a, x$b))
    Condition
      Error in `join_by()`:
      ! Expressions containing `overlaps()` must reference the same table for the right-hand side lower and upper bounds.
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
      join_by(closest())
    Condition
      Error:
      ! Expressions using `closest()` can't contain missing arguments.
      x Argument `expr` is missing.

---

    Code
      join_by(`$`(x) > y)
    Condition
      Error:
      ! Expressions using `$` can't contain missing arguments.
      x Argument `name` is missing.

---

    Code
      join_by(closest(a >= b, 1))
    Condition
      Error in `closest()`:
      ! unused argument (1)

---

    Code
      join_by(closest(a == b))
    Condition
      Error in `join_by()`:
      ! The expression used in `closest()` can't use `==`.
      i Expression 1 is `closest(a == b)`.

---

    Code
      join_by(closest(x))
    Condition
      Error in `join_by()`:
      ! The first argument of `closest()` must be an expression.
      i Expression 1 is `closest(x)`.

---

    Code
      join_by(closest(1))
    Condition
      Error in `join_by()`:
      ! The first argument of `closest()` must be an expression.
      i Expression 1 is `closest(1)`.

---

    Code
      join_by(closest(x + y))
    Condition
      Error in `join_by()`:
      ! The expression used in `closest()` must use one of: `>=`, `>`, `<=`, or `<`.
      i Expression 1 is `closest(x + y)`.

---

    Code
      join_by(between(x, lower, upper, bounds = 1))
    Condition
      Error:
      ! `bounds` must be a string or character vector.

---

    Code
      join_by(between(x, lower, upper, bounds = "a"))
    Condition
      Error:
      ! `bounds` must be one of "[]", "[)", "(]", or "()", not "a".

---

    Code
      join_by(overlaps(x, y, lower, upper, bounds = 1))
    Condition
      Error:
      ! `bounds` must be a string or character vector.

---

    Code
      join_by(overlaps(x, y, lower, upper, bounds = "a"))
    Condition
      Error:
      ! `bounds` must be one of "[]", "[)", "(]", or "()", not "a".

---

    Code
      join_by(between(x, lower, upper, foo = 1))
    Condition
      Error:
      ! `...` must be empty.
      i Non-empty dots were detected inside `between()`.

---

    Code
      join_by(overlaps(x, y, lower, upper, foo = 1))
    Condition
      Error:
      ! `...` must be empty.
      i Non-empty dots were detected inside `overlaps()`.

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
      i Use `cross_join()` to perform a cross-join.

