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
      ! Can't transform a data frame with missing names.

# summarise() messages about implicit `.groups` default

    Code
      summarise(group_by(df, x))
    Output
      # A tibble: 1 x 1
            x
        <dbl>
      1     1

---

    Code
      summarise(rowwise(df))
    Output
      # A tibble: 1 x 0

---

    Code
      summarise(group_by(df, x, y))
    Message
      `summarise()` has regrouped the output.
      i Summaries were computed grouped by x and y.
      i Output is grouped by x.
      i Use `summarise(.groups = "drop_last")` to silence this message.
      i Use `summarise(.by = c(x, y))` for per-operation grouping (`?dplyr::dplyr_by`) instead.
    Output
      # A tibble: 1 x 2
      # Groups:   x [1]
            x     y
        <dbl> <dbl>
      1     1     2

---

    Code
      summarise(rowwise(df, x, y))
    Message
      `summarise()` has converted the output from a rowwise data frame to a grouped data frame.
      i Summaries were computed rowwise.
      i Output is grouped by x and y.
      i Use `summarise(.groups = "keep")` to silence this message.
    Output
      # A tibble: 1 x 2
      # Groups:   x, y [1]
            x     y
        <dbl> <dbl>
      1     1     2

# summarise() respects `dplyr.summarise.inform = FALSE`

    Code
      eval_global(summarise(group_by(tibble(x = 1, y = 2), x, y)))
    Output
      # A tibble: 1 x 2
      # Groups:   x [1]
            x     y
        <dbl> <dbl>
      1     1     2

---

    Code
      eval_global(summarise(rowwise(tibble(x = 1, y = 2), x, y)))
    Output
      # A tibble: 1 x 2
      # Groups:   x, y [1]
            x     y
        <dbl> <dbl>
      1     1     2

# summarise() gives meaningful errors

    Code
      (expect_error(summarise(tibble(x = 1, y = c(1, 2, 2), z = runif(3)), a = rlang::env(
        a = 1))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = rlang::env(a = 1)`.
      Caused by error:
      ! `a` must be a vector, not an environment.
    Code
      (expect_error(summarise(group_by(tibble(x = 1, y = c(1, 2, 2), z = runif(3)), x,
      y), a = rlang::env(a = 1))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = rlang::env(a = 1)`.
      i In group 1: `x = 1`, `y = 1`.
      Caused by error:
      ! `a` must be a vector, not an environment.
    Code
      (expect_error(summarise(group_by(tibble(x = 1, y = c(1, 2, 2), y2 = c(1, 2, 2),
      z = runif(3)), x, y, y2), a = rlang::env(a = 1))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = rlang::env(a = 1)`.
      i In group 1: `x = 1`, `y = 1`, `y2 = 1`.
      Caused by error:
      ! `a` must be a vector, not an environment.
    Code
      (expect_error(summarise(rowwise(tibble(x = 1, y = c(1, 2, 2), z = runif(3))),
      a = lm(y ~ x))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = lm(y ~ x)`.
      i In row 1.
      Caused by error:
      ! `a` must be a vector, not a <lm> object.
      i Did you mean: `a = list(lm(y ~ x))` ?
    Code
      (expect_error(summarise(group_by(tibble(id = 1:2, a = list(1, "2")), id), a = a[[
        1]])))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = a[[1]]`.
      Caused by error:
      ! `a` must return compatible vectors across groups.
      i Result of type <double> for group 1: `id = 1`.
      i Result of type <character> for group 2: `id = 2`.
    Code
      (expect_error(summarise(rowwise(tibble(id = 1:2, a = list(1, "2"))), a = a[[1]]))
      )
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = a[[1]]`.
      Caused by error:
      ! `a` must return compatible vectors across groups.
    Code
      (expect_error(summarise(group_by(data.frame(x = 1:2, g = 1:2), g), x = if (g ==
        1) 42)))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `x = if (g == 1) 42`.
      i In group 2: `g = 2`.
      Caused by error:
      ! `x` must return compatible vectors across groups.
      x Can't combine NULL and non NULL results.
    Code
      (expect_error(summarise(group_by(data.frame(x = 1:2, g = 1:2), g), x = if (g ==
        2) 42)))
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
      (expect_error(summarise(tibble(x = 1, x = 1, .name_repair = "minimal"), x)))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't transform a data frame with duplicate names.
    Code
      (expect_error(summarise(tibble(), stop("{"))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `stop("{")`.
      Caused by error:
      ! {
    Code
      (expect_error(summarise(group_by(tibble(a = 1, b = "{value:1, unit:a}"), b), a = stop(
        "!"))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `a = stop("!")`.
      i In group 1: `b = "{value:1, unit:a}"`.
      Caused by error:
      ! !

# non-summary results are defunct in favor of `reframe()` (#6382, #7761)

    Code
      out <- summarise(df, x = which(x < 3))
    Condition
      Error in `summarise()`:
      i In argument: `x = which(x < 3)`.
      Caused by error:
      ! `x` must be size 1, not 2.
      i To return more or less than 1 row per group, use `reframe()`.

---

    Code
      out <- summarise(df, x = which(x < 3), .by = g)
    Condition
      Error in `summarise()`:
      i In argument: `x = which(x < 3)`.
      i In group 1: `g = 1`.
      Caused by error:
      ! `x` must be size 1, not 2.
      i To return more or less than 1 row per group, use `reframe()`.

---

    Code
      out <- summarise(gdf, x = which(x < 3))
    Condition
      Error in `summarise()`:
      i In argument: `x = which(x < 3)`.
      i In group 1: `g = 1`.
      Caused by error:
      ! `x` must be size 1, not 2.
      i To return more or less than 1 row per group, use `reframe()`.

---

    Code
      out <- summarise(rdf, x = which(x < 3))
    Condition
      Error in `summarise()`:
      i In argument: `x = which(x < 3)`.
      i In row 3.
      Caused by error:
      ! `x` must be size 1, not 0.
      i To return more or less than 1 row per group, use `reframe()`.

---

    Code
      summarise(tibble(), x = 1, y = 1:3, z = 1)
    Condition
      Error in `summarise()`:
      i In argument: `y = 1:3`.
      Caused by error:
      ! `y` must be size 1, not 3.
      i To return more or less than 1 row per group, use `reframe()`.

---

    Code
      gf <- group_by(tibble(a = 1:2), a)
      summarise(gf, x = 1, y = 1:3, z = 1)
    Condition
      Error in `summarise()`:
      i In argument: `y = 1:3`.
      i In group 1: `a = 1`.
      Caused by error:
      ! `y` must be size 1, not 3.
      i To return more or less than 1 row per group, use `reframe()`.

---

    Code
      gf <- group_by(tibble(a = 1:2), a)
      summarise(gf, x = seq_len(a), y = 1)
    Condition
      Error in `summarise()`:
      i In argument: `x = seq_len(a)`.
      i In group 2: `a = 2`.
      Caused by error:
      ! `x` must be size 1, not 2.
      i To return more or less than 1 row per group, use `reframe()`.

