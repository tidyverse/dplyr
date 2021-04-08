# summarise() gives meaningful errors

    Code
      tibble(x = 1, y = 2) %>% group_by(x, y) %>% summarise()
    Message <message>
      `summarise()` has grouped output by 'x'. You can override using the `.groups` argument.
    Output
      # A tibble: 1 x 2
      # Groups:   x [1]
            x     y
        <dbl> <dbl>
      1     1     2

---

    Code
      tibble(x = 1, y = 2) %>% group_by(x, y) %>% summarise(z = c(2, 2))
    Message <message>
      `summarise()` has grouped output by 'x', 'y'. You can override using the `.groups` argument.
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
    Message <message>
      `summarise()` has grouped output by 'x', 'y'. You can override using the `.groups` argument.
    Output
      # A tibble: 1 x 2
      # Groups:   x, y [1]
            x     y
        <dbl> <dbl>
      1     1     2

---

    Code
      tibble(x = 1, y = 2) %>% rowwise() %>% summarise()
    Message <message>
      `summarise()` has ungrouped output. You can override using the `.groups` argument.
    Output
      # A tibble: 1 x 0

---

    Code
      tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>% summarise(a = rlang::env(a = 1))
    Error <dplyr_error>
      Problem with `summarise()` column `a`.
      x Input `a` must be a vector, not an environment.
      i Input `a` is `rlang::env(a = 1)`.

---

    Code
      tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>% group_by(x, y) %>% summarise(a = rlang::env(
        a = 1))
    Error <dplyr_error>
      Problem with `summarise()` column `a`.
      x Input `a` must be a vector, not an environment.
      i Input `a` is `rlang::env(a = 1)`.
      i The error occurred in group 1: x = 1, y = 1.

---

    Code
      tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>% rowwise() %>% summarise(a = lm(
        y ~ x))
    Error <dplyr_error>
      Problem with `summarise()` column `a`.
      x Input `a` must be a vector, not a `lm` object.
      i Did you mean: `a = list(lm(y ~ x))` ?
      i Input `a` is `lm(y ~ x)`.
      i The error occurred in row 1.

---

    Code
      tibble(id = 1:2, a = list(1, "2")) %>% group_by(id) %>% summarise(a = a[[1]])
    Error <dplyr_error>
      Problem with `summarise()` column `a`.
      x Input `a` must return compatible vectors across groups
      i Result type for group 1 (id = 1): <double>.
      i Result type for group 2 (id = 2): <character>.
      i Input `a` is `a[[1]]`.

---

    Code
      tibble(id = 1:2, a = list(1, "2")) %>% rowwise() %>% summarise(a = a[[1]])
    Error <dplyr_error>
      Problem with `summarise()` column `a`.
      x Input `a` must return compatible vectors across groups
      i Input `a` is `a[[1]]`.

---

    Code
      tibble(z = 1) %>% summarise(x = 1:3, y = 1:2)
    Error <dplyr_error>
      Problem with `summarise()` column `y`.
      x Input `y` must be size 3 or 1, not 2.
      i An earlier column had size 3.
      i Input `y` is `1:2`.

---

    Code
      tibble(z = 1:2) %>% group_by(z) %>% summarise(x = 1:3, y = 1:2)
    Error <dplyr_error>
      Problem with `summarise()` column `y`.
      x Input `y` must be size 3 or 1, not 2.
      i An earlier column had size 3.
      i Input `y` is `1:2`.
      i The error occurred in group 1: z = 1.

---

    Code
      tibble(z = c(1, 3)) %>% group_by(z) %>% summarise(x = seq_len(z), y = 1:2)
    Error <dplyr_error>
      Problem with `summarise()` column `y`.
      x Input `y` must be size 3 or 1, not 2.
      i An earlier column had size 3.
      i Input `y` is `1:2`.
      i The error occurred in group 2: z = 3.

---

    Code
      data.frame(x = 1:2, g = 1:2) %>% group_by(g) %>% summarise(x = if (g == 1) 42)
    Error <dplyr_error>
      Problem with `summarise()` column `x`.
      x `x` must return compatible vectors across groups.
      i Cannot combine NULL and non NULL results.
      i Input `x` is `if (g == 1) 42`.

---

    Code
      summarise(mtcars, a = mean(not_there))
    Error <dplyr_error>
      Problem with `summarise()` column `a`.
      x object 'not_there' not found
      i Input `a` is `mean(not_there)`.

---

    Code
      summarise(group_by(mtcars, cyl), a = mean(not_there))
    Error <dplyr_error>
      Problem with `summarise()` column `a`.
      x object 'not_there' not found
      i Input `a` is `mean(not_there)`.
      i The error occurred in group 1: cyl = 4.

---

    Code
      summarise(tibble(a = 1), c = .data$b)
    Error <dplyr_error>
      Problem with `summarise()` column `c`.
      x Column `b` not found in `.data`
      i Input `c` is `.data$b`.

---

    Code
      summarise(group_by(tibble(a = 1:3), a), c = .data$b)
    Error <dplyr_error>
      Problem with `summarise()` column `c`.
      x Column `b` not found in `.data`
      i Input `c` is `.data$b`.
      i The error occurred in group 1: a = 1.

---

    Code
      tibble(x = 1, x = 1, .name_repair = "minimal") %>% summarise(x)
    Error <rlang_error>
      Can't transform a data frame with duplicate names.

---

    Code
      tibble() %>% summarise(stop("{"))
    Error <dplyr_error>
      Problem with `summarise()` column `..1`.
      x {
      i Input `..1` is `stop("{")`.

---

    Code
      tibble(a = 1, b = "{value:1, unit:a}") %>% group_by(b) %>% summarise(a = stop(
        "!"))
    Error <dplyr_error>
      Problem with `summarise()` column `a`.
      x !
      i Input `a` is `stop("!")`.
      i The error occurred in group 1: b = "{value:1, unit:a}".

