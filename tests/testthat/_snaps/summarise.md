# summarise() gives meaningful errors

    Code
      tibble(x = 1, y = 2) %>% group_by(x, y) %>% summarise()
    Output
      # A tibble: 1 x 2
      # Groups:   x [1]
            x     y
        <dbl> <dbl>
      1     1     2

---

    Code
      tibble(x = 1, y = 2) %>% group_by(x, y) %>% summarise(z = c(2, 2))
    Output
      # A tibble: 2 x 3
      # Groups:   x, y [1]
            x     y     z
        <dbl> <dbl> <dbl>
      1     1     2     2
      2     1     2     2

---

    Code
      tibble(x = 1, y = 2) %>% rowwise(x, y) %>% summarise()
    Output
      # A tibble: 1 x 2
      # Groups:   x, y [1]
            x     y
        <dbl> <dbl>
      1     1     2

---

    Code
      tibble(x = 1, y = 2) %>% rowwise() %>% summarise()
    Output
      # A tibble: 1 x 0

---

    Problem with `summarise()` input `a`.
    x Input `a` must be a vector, not an environment.
    i Input `a` is `rlang::env(a = 1)`.

---

    Problem with `summarise()` input `a`.
    x Input `a` must be a vector, not an environment.
    i Input `a` is `rlang::env(a = 1)`.
    i The error occurred in group 1: x = 1, y = 1.

---

    Problem with `summarise()` input `a`.
    x Input `a` must be a vector, not a `lm` object.
    i Did you mean: `a = list(lm(y ~ x))` ?
    i Input `a` is `lm(y ~ x)`.
    i The error occurred in row 1.

---

    Problem with `summarise()` input `a`.
    x Input `a` must return compatible vectors across groups
    i Result type for group 1 (id = 1): <double>.
    i Result type for group 2 (id = 2): <character>.
    i Input `a` is `a[[1]]`.

---

    Problem with `summarise()` input `a`.
    x Input `a` must return compatible vectors across groups
    i Input `a` is `a[[1]]`.

---

    Problem with `summarise()` input `y`.
    x Input `y` must be size 3 or 1, not 2.
    i An earlier column had size 3.
    i Input `y` is `1:2`.

---

    Problem with `summarise()` input `y`.
    x Input `y` must be size 3 or 1, not 2.
    i An earlier column had size 3.
    i Input `y` is `1:2`.
    i The error occurred in group 1: z = 1.

---

    Problem with `summarise()` input `y`.
    x Input `y` must be size 3 or 1, not 2.
    i An earlier column had size 3.
    i Input `y` is `1:2`.
    i The error occurred in group 2: z = 3.

---

    Problem with `summarise()` input `x`.
    x `x` must return compatible vectors across groups.
    i Cannot combine NULL and non NULL results.
    i Input `x` is `if (g == 1) 42`.

---

    Problem with `summarise()` input `a`.
    x object 'not_there' not found
    i Input `a` is `mean(not_there)`.

---

    Problem with `summarise()` input `a`.
    x object 'not_there' not found
    i Input `a` is `mean(not_there)`.
    i The error occurred in group 1: cyl = 4.

---

    Problem with `summarise()` input `c`.
    x Column `b` not found in `.data`
    i Input `c` is `.data$b`.

---

    Problem with `summarise()` input `c`.
    x Column `b` not found in `.data`
    i Input `c` is `.data$b`.
    i The error occurred in group 1: a = 1.

---

    Can't transform a data frame with duplicate names.

---

    Problem with `summarise()` input `..1`.
    x {
    i Input `..1` is `stop("{")`.

---

    Problem with `summarise()` input `a`.
    x !
    i Input `a` is `stop("!")`.
    i The error occurred in group 1: b = "{value:1, unit:a}".

