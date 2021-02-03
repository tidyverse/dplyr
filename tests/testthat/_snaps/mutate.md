# mutate() give meaningful errors

    Problem with `mutate()` input `a`.
    x object 'y' not found
    i Input `a` is `sum(y)`.

---

    Problem with `mutate()` input `a`.
    x object 'y' not found
    i Input `a` is `sum(y)`.
    i The error occurred in group 1: x = 1.

---

    Problem with `mutate()` input `y`.
    x Input `y` must be a vector, not a function.
    i Input `y` is `mean`.

---

    Problem with `mutate()` input `out`.
    x Input `out` must be a vector, not an environment.
    i Input `out` is `env(a = 1)`.

---

    Problem with `mutate()` input `out`.
    x Input `out` must be a vector, not an environment.
    i Input `out` is `env(a = 1)`.
    i The error occurred in group 1: g = 1.

---

    Problem with `mutate()` input `out`.
    x Input `out` must be a vector, not a function.
    i Input `out` is `rnorm`.
    i Did you mean: `out = list(rnorm)` ?
    i The error occurred in row 1.

---

    Problem with `mutate()` input `val`.
    x Input `val` must return compatible vectors across groups
    i Input `val` is `ifelse(x < 3, "foo", 2)`.
    i Result type for group 1 (x = 1): <character>.
    i Result type for group 3 (x = 3): <double>.

---

    Problem with `mutate()` input `..1`.
    x `..1` must return compatible vectors across groups.
    i Input `..1` is `if (a == 1) NULL else "foo"`.
    i Cannot combine NULL and non NULL results.

---

    Problem with `mutate()` input `int`.
    x Input `int` can't be recycled to size 4.
    i Input `int` is `1:5`.
    i Input `int` must be size 4 or 1, not 5.

---

    Problem with `mutate()` input `int`.
    x Input `int` can't be recycled to size 2.
    i Input `int` is `1:5`.
    i Input `int` must be size 2 or 1, not 5.
    i The error occurred in group 1: x = 2.

---

    Problem with `mutate()` input `int`.
    x Input `int` can't be recycled to size 1.
    i Input `int` is `1:5`.
    i Input `int` must be size 1, not 5.
    i The error occurred in group 1: x = 2.

---

    Problem with `mutate()` input `int`.
    x Input `int` can't be recycled to size 1.
    i Input `int` is `1:5`.
    i Input `int` must be size 1, not 5.
    i Did you mean: `int = list(1:5)` ?
    i The error occurred in row 1.

---

    Problem with `mutate()` input `y2`.
    x Input `y2` can't be recycled to size 1.
    i Input `y2` is `y`.
    i Input `y2` must be size 1, not 3.
    i Did you mean: `y2 = list(y)` ?
    i The error occurred in row 1.

---

    Problem with `mutate()` input `y`.
    x Input `y` can't be recycled to size 10.
    i Input `y` is `1:2`.
    i Input `y` must be size 10 or 1, not 2.

---

    Problem with `mutate()` input `c`.
    x Column `b` not found in `.data`
    i Input `c` is `.data$b`.

---

    Problem with `mutate()` input `c`.
    x Column `b` not found in `.data`
    i Input `c` is `.data$b`.
    i The error occurred in group 1: a = 1.

---

    Obsolete data mask.
    x Too late to resolve `x` after the end of `dplyr::mutate()`.
    i Did you save an object that uses `x` lazily in a column in the `dplyr::mutate()` expression ?

---

    Problem with `mutate()` input `..1`.
    x {
    i Input `..1` is `stop("{")`.

