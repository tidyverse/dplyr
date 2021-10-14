# summarise() gives meaningful errors

    Code
      tibble(x = 1, y = 2) %>% group_by(x, y) %>% summarise()
    Message <rlang_message>
      `summarise()` has grouped output by 'x'. You can override using the `.groups` argument.
    Output
      # A tibble: 1 x 2
      # Groups:   x [1]
            x     y
        <dbl> <dbl>
      1     1     2
    Code
      tibble(x = 1, y = 2) %>% group_by(x, y) %>% summarise(z = c(2, 2))
    Message <rlang_message>
      `summarise()` has grouped output by 'x', 'y'. You can override using the `.groups` argument.
    Output
      # A tibble: 2 x 3
      # Groups:   x, y [1]
            x     y     z
        <dbl> <dbl> <dbl>
      1     1     2     2
      2     1     2     2
    Code
      tibble(x = 1, y = 2) %>% rowwise(x, y) %>% summarise()
    Message <rlang_message>
      `summarise()` has grouped output by 'x', 'y'. You can override using the `.groups` argument.
    Output
      # A tibble: 1 x 2
      # Groups:   x, y [1]
            x     y
        <dbl> <dbl>
      1     1     2
    Code
      tibble(x = 1, y = 2) %>% rowwise() %>% summarise()
    Output
      # A tibble: 1 x 0

---

    Code
      (expect_error(tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>% summarise(a = rlang::env(
        a = 1))))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `a = rlang::env(a = 1)`.
      i `a = rlang::env(a = 1)`.
      x `a` must be a vector, not an environment.
    Code
      (expect_error(tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>% group_by(x, y) %>%
        summarise(a = rlang::env(a = 1))))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `a = rlang::env(a = 1)`.
      i `a = rlang::env(a = 1)`.
      x `a` must be a vector, not an environment.
      i The error occurred in group 1: x = 1, y = 1.
    Code
      (expect_error(tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>% rowwise() %>%
        summarise(a = lm(y ~ x))))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `a = lm(y ~ x)`.
      i `a = lm(y ~ x)`.
      x `a` must be a vector, not a `lm` object.
      i Did you mean: `a = list(lm(y ~ x))` ?
      i The error occurred in row 1.
    Code
      (expect_error(tibble(id = 1:2, a = list(1, "2")) %>% group_by(id) %>% summarise(
        a = a[[1]])))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `a = a[[1]]`.
      i `a = a[[1]]`.
      x `a` must return compatible vectors across groups
      i Result type for group 1 (id = 1): <double>.
      i Result type for group 2 (id = 2): <character>.
    Code
      (expect_error(tibble(id = 1:2, a = list(1, "2")) %>% rowwise() %>% summarise(a = a[[
        1]])))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `a = a[[1]]`.
      i `a = a[[1]]`.
      x `a` must return compatible vectors across groups
    Code
      (expect_error(tibble(z = 1) %>% summarise(x = 1:3, y = 1:2)))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `y = 1:2`.
      i `y = 1:2`.
      x `y` must be size 3 or 1, not 2.
      i An earlier column had size 3.
    Code
      (expect_error(tibble(z = 1:2) %>% group_by(z) %>% summarise(x = 1:3, y = 1:2)))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `y = 1:2`.
      i `y = 1:2`.
      x `y` must be size 3 or 1, not 2.
      i An earlier column had size 3.
      i The error occurred in group 1: z = 1.
    Code
      (expect_error(tibble(z = c(1, 3)) %>% group_by(z) %>% summarise(x = seq_len(z),
      y = 1:2)))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `y = 1:2`.
      i `y = 1:2`.
      x `y` must be size 3 or 1, not 2.
      i An earlier column had size 3.
      i The error occurred in group 2: z = 3.
    Code
      (expect_error(data.frame(x = 1:2, g = 1:2) %>% group_by(g) %>% summarise(x = if (
        g == 1) 42)))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `x = if (g == 1) 42`.
      i `x = if (g == 1) 42`.
      x `x` must return compatible vectors across groups.
      i Cannot combine NULL and non NULL results.
    Code
      (expect_error(summarise(mtcars, a = mean(not_there))))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `a = mean(not_there)`.
      i `a = mean(not_there)`.
      x object 'not_there' not found
    Code
      (expect_error(summarise(group_by(mtcars, cyl), a = mean(not_there))))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `a = mean(not_there)`.
      i `a = mean(not_there)`.
      x object 'not_there' not found
      i The error occurred in group 1: cyl = 4.
    Code
      (expect_error(summarise(tibble(a = 1), c = .data$b)))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `c = .data$b`.
      i `c = .data$b`.
      x Column `b` not found in `.data`.
    Code
      (expect_error(summarise(group_by(tibble(a = 1:3), a), c = .data$b)))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `c = .data$b`.
      i `c = .data$b`.
      x Column `b` not found in `.data`.
      i The error occurred in group 1: a = 1.
    Code
      (expect_error(tibble(x = 1, x = 1, .name_repair = "minimal") %>% summarise(x)))
    Output
      <error/rlang_error>
      Error in `initialize()`: Can't transform a data frame with duplicate names.
    Code
      (expect_error(tibble() %>% summarise(stop("{"))))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `..1 = stop("{")`.
      i `..1 = stop("{")`.
      x {
    Code
      (expect_error(tibble(a = 1, b = "{value:1, unit:a}") %>% group_by(b) %>%
        summarise(a = stop("!"))))
    Output
      <error/dplyr_error>
      Error in `summarise()`: Problem while computing `a = stop("!")`.
      i `a = stop("!")`.
      x !
      i The error occurred in group 1: b = "{value:1, unit:a}".

