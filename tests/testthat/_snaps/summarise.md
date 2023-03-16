# can't overwrite column active bindings (#6666)

    Code
      summarise(df, y = {
        x <<- x + 2L
        mean(x)
      })
    Condition
      Error in `summarise()`:
      i In argument: `y = { ... }`.
      Caused by error:
      ! unused argument (base::quote(3:6))

---

    Code
      summarise(df, .by = g, y = {
        x <<- x + 2L
        mean(x)
      })
    Condition
      Error in `summarise()`:
      i In argument: `y = { ... }`.
      i In group 1: `g = 1`.
      Caused by error:
      ! unused argument (base::quote(3:4))

---

    Code
      summarise(gdf, y = {
        x <<- x + 2L
        mean(x)
      })
    Condition
      Error in `summarise()`:
      i In argument: `y = { ... }`.
      i In group 1: `g = 1`.
      Caused by error:
      ! unused argument (base::quote(3:4))

# can't use `.by` with `.groups`

    Code
      summarise(df, .by = x, .groups = "drop")
    Condition
      Error in `summarise()`:
      ! Can't supply both `.by` and `.groups`.

# catches `.by` with grouped-df

    Code
      summarise(gdf, .by = x)
    Condition
      Error in `summarise()`:
      ! Can't supply `.by` when `.data` is a grouped data frame.

# catches `.by` with rowwise-df

    Code
      summarise(rdf, .by = x)
    Condition
      Error in `summarise()`:
      ! Can't supply `.by` when `.data` is a rowwise data frame.

# `summarise()` doesn't allow data frames with missing or empty names (#6758)

    Code
      summarise(df1)
    Condition
      Error in `summarise()`:
      ! Can't transform a data frame with `NA` or `""` names.

---

    Code
      summarise(df2)
    Condition
      Error in `summarise()`:
      ! Can't transform a data frame with `NA` or `""` names.

# summarise() gives meaningful errors

    Code
      tibble(x = 1, y = 2) %>% group_by(x, y) %>% summarise()
    Message
      `summarise()` has grouped output by 'x'. You can override using the `.groups` argument.
    Output
      # A tibble: 1 x 2
      # Groups:   x [1]
            x     y
        <dbl> <dbl>
      1     1     2
    Code
      tibble(x = 1, y = 2) %>% rowwise(x, y) %>% summarise()
    Message
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
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = rlang::env(a = 1)`.
      Caused by error:
      ! `a` must be a vector, not an environment.
    Code
      (expect_error(tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>% group_by(x, y) %>%
        summarise(a = rlang::env(a = 1))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = rlang::env(a = 1)`.
      i In group 1: `x = 1`, `y = 1`.
      Caused by error:
      ! `a` must be a vector, not an environment.
    Code
      (expect_error(tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>% rowwise() %>%
        summarise(a = lm(y ~ x))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = lm(y ~ x)`.
      i In row 1.
      Caused by error:
      ! `a` must be a vector, not a <lm> object.
      i Did you mean: `a = list(lm(y ~ x))` ?
    Code
      (expect_error(tibble(id = 1:2, a = list(1, "2")) %>% group_by(id) %>% summarise(
        a = a[[1]])))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = a[[1]]`.
      Caused by error:
      ! `a` must return compatible vectors across groups.
      i Result of type <double> for group 1: `id = 1`.
      i Result of type <character> for group 2: `id = 2`.
    Code
      (expect_error(tibble(id = 1:2, a = list(1, "2")) %>% rowwise() %>% summarise(a = a[[
        1]])))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = a[[1]]`.
      Caused by error:
      ! `a` must return compatible vectors across groups.
    Code
      (expect_error(tibble(z = 1) %>% summarise(x = 1:3, y = 1:2)))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't recycle `y = 1:2`.
      Caused by error:
      ! `y` must be size 3 or 1, not 2.
      i An earlier column had size 3.
    Code
      (expect_error(tibble(z = 1:2) %>% group_by(z) %>% summarise(x = 1:3, y = 1:2)))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't recycle `y = 1:2`.
      i In group 1: `z = 1`.
      Caused by error:
      ! `y` must be size 3 or 1, not 2.
      i An earlier column had size 3.
    Code
      (expect_error(tibble(z = c(1, 3)) %>% group_by(z) %>% summarise(x = seq_len(z),
      y = 1:2)))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't recycle `y = 1:2`.
      i In group 2: `z = 3`.
      Caused by error:
      ! `y` must be size 3 or 1, not 2.
      i An earlier column had size 3.
    Code
      (expect_error(data.frame(x = 1:2, g = 1:2) %>% group_by(g) %>% summarise(x = if (
        g == 1) 42)))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `x = if (g == 1) 42`.
      i In group 2: `g = 2`.
      Caused by error:
      ! `x` must return compatible vectors across groups.
      x Can't combine NULL and non NULL results.
    Code
      (expect_error(data.frame(x = 1:2, g = 1:2) %>% group_by(g) %>% summarise(x = if (
        g == 2) 42)))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `x = if (g == 2) 42`.
      i In group 1: `g = 1`.
      Caused by error:
      ! `x` must return compatible vectors across groups.
      x Can't combine NULL and non NULL results.
    Code
      (expect_error(summarise(tibble(a = 1), c = .data$b)))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `c = .data$b`.
      Caused by error in `.data$b`:
      ! Column `b` not found in `.data`.
    Code
      (expect_error(summarise(group_by(tibble(a = 1:3), a), c = .data$b)))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `c = .data$b`.
      i In group 1: `a = 1`.
      Caused by error in `.data$b`:
      ! Column `b` not found in `.data`.
    Code
      (expect_error(tibble(x = 1, x = 1, .name_repair = "minimal") %>% summarise(x)))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't transform a data frame with duplicate names.
    Code
      (expect_error(tibble() %>% summarise(stop("{"))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `stop("{")`.
      Caused by error:
      ! {
    Code
      (expect_error(tibble(a = 1, b = "{value:1, unit:a}") %>% group_by(b) %>%
        summarise(a = stop("!"))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = stop("!")`.
      i In group 1: `b = "{value:1, unit:a}"`.
      Caused by error:
      ! !

# non-summary results are deprecated in favor of `reframe()` (#6382)

    Code
      out <- summarise(df, x = which(x < 3))
    Condition
      Warning:
      Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr 1.1.0.
      i Please use `reframe()` instead.
      i When switching from `summarise()` to `reframe()`, remember that `reframe()` always returns an ungrouped data frame and adjust accordingly.

---

    Code
      out <- summarise(df, x = which(x < 3), .by = g)
    Condition
      Warning:
      Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr 1.1.0.
      i Please use `reframe()` instead.
      i When switching from `summarise()` to `reframe()`, remember that `reframe()` always returns an ungrouped data frame and adjust accordingly.

---

    Code
      out <- summarise(gdf, x = which(x < 3))
    Condition
      Warning:
      Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr 1.1.0.
      i Please use `reframe()` instead.
      i When switching from `summarise()` to `reframe()`, remember that `reframe()` always returns an ungrouped data frame and adjust accordingly.

---

    Code
      out <- summarise(rdf, x = which(x < 3))
    Condition
      Warning:
      Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr 1.1.0.
      i Please use `reframe()` instead.
      i When switching from `summarise()` to `reframe()`, remember that `reframe()` always returns an ungrouped data frame and adjust accordingly.

