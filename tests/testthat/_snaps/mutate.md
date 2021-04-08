# mutate() give meaningful errors

    Code
      tbl %>% mutate(y = NULL, a = sum(y))
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `a`.
      i `a = sum(y)`.
      x object 'y' not found

---

    Code
      tbl %>% group_by(x) %>% mutate(y = NULL, a = sum(y))
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `a`.
      i `a = sum(y)`.
      x object 'y' not found
      i The error occurred in group 1: x = 1.

---

    Code
      tibble(x = 1) %>% mutate(y = mean)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `y`.
      i `y = mean`.
      x Input `y` must be a vector, not a function.

---

    Code
      df %>% mutate(out = env(a = 1))
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `out`.
      i `out = env(a = 1)`.
      x Input `out` must be a vector, not an environment.

---

    Code
      df %>% group_by(g) %>% mutate(out = env(a = 1))
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `out`.
      i `out = env(a = 1)`.
      x Input `out` must be a vector, not an environment.
      i The error occurred in group 1: g = 1.

---

    Code
      df %>% rowwise() %>% mutate(out = rnorm)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `out`.
      i `out = rnorm`.
      x Input `out` must be a vector, not a function.
      i Did you mean: `out = list(rnorm)` ?
      i The error occurred in row 1.

---

    Code
      data.frame(x = rep(1:5, each = 3)) %>% group_by(x) %>% mutate(val = ifelse(x <
      3, "foo", 2))
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `val`.
      i `val = ifelse(x < 3, "foo", 2)`.
      x Input `val` must return compatible vectors across groups
      i Result type for group 1 (x = 1): <character>.
      i Result type for group 3 (x = 3): <double>.

---

    Code
      tibble(a = 1:3, b = 4:6) %>% group_by(a) %>% mutate(if (a == 1) NULL else "foo")
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `..1`.
      i `..1 = if (a == 1) NULL else "foo"`.
      x `..1` must return compatible vectors across groups.
      i Cannot combine NULL and non NULL results.

---

    Code
      data.frame(x = c(2, 2, 3, 3)) %>% mutate(int = 1:5)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `int`.
      i `int = 1:5`.
      i Input `int` must be size 4 or 1, not 5.

---

    Code
      data.frame(x = c(2, 2, 3, 3)) %>% group_by(x) %>% mutate(int = 1:5)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `int`.
      i `int = 1:5`.
      i Input `int` must be size 2 or 1, not 5.
      i The error occurred in group 1: x = 2.

---

    Code
      data.frame(x = c(2, 3, 3)) %>% group_by(x) %>% mutate(int = 1:5)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `int`.
      i `int = 1:5`.
      i Input `int` must be size 1, not 5.
      i The error occurred in group 1: x = 2.

---

    Code
      data.frame(x = c(2, 2, 3, 3)) %>% rowwise() %>% mutate(int = 1:5)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `int`.
      i `int = 1:5`.
      i Input `int` must be size 1, not 5.
      i Did you mean: `int = list(1:5)` ?
      i The error occurred in row 1.

---

    Code
      tibble(y = list(1:3, "a")) %>% rowwise() %>% mutate(y2 = y)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `y2`.
      i `y2 = y`.
      i Input `y2` must be size 1, not 3.
      i Did you mean: `y2 = list(y)` ?
      i The error occurred in row 1.

---

    Code
      data.frame(x = 1:10) %>% mutate(y = 11:20, y = 1:2)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `y`.
      i `y = 1:2`.
      i Input `y` must be size 10 or 1, not 2.

---

    Code
      tibble(a = 1) %>% mutate(c = .data$b)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `c`.
      i `c = .data$b`.
      x Column `b` not found in `.data`

---

    Code
      tibble(a = 1:3) %>% group_by(a) %>% mutate(c = .data$b)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `c`.
      i `c = .data$b`.
      x Column `b` not found in `.data`
      i The error occurred in group 1: a = 1.

---

    Code
      eval_tidy(res$z[[1]])
    Error <rlang_error>
      Obsolete data mask.
      x Too late to resolve `x` after the end of `dplyr::mutate()`.
      i Did you save an object that uses `x` lazily in a column in the `dplyr::mutate()` expression ?

---

    Code
      tibble() %>% mutate(stop("{"))
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `..1`.
      i `..1 = stop("{")`.
      x {

