# mutate() supports constants (#6056, #6305)

    Code
      (expect_error(df %>% mutate(z = !!z)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `z = <int>`.
      Caused by error:
      ! Inlined constant `z` must be size 10 or 1, not 5.
    Code
      (expect_error(df %>% group_by(g) %>% mutate(z = !!z)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `z = <int>`.
      Caused by error:
      ! Inlined constant `z` must be size 10 or 1, not 5.
    Code
      (expect_error(df %>% rowwise() %>% mutate(z = !!z)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `z = <int>`.
      Caused by error:
      ! Inlined constant `z` must be size 10 or 1, not 5.

---

    Code
      (expect_error(df %>% group_by(g) %>% mutate(y = .env$y)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `y = .env$y`.
      i In group 1: `g = 1`.
      Caused by error:
      ! `y` must be size 5 or 1, not 10.
    Code
      (expect_error(df %>% rowwise() %>% mutate(y = .env$y)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `y = .env$y`.
      i In row 1.
      Caused by error:
      ! `y` must be size 1, not 10.
      i Did you mean: `y = list(.env$y)` ?

# can't overwrite column active bindings (#6666)

    Code
      mutate(df, y = {
        x <<- 2
        x
      })
    Condition
      Error in `mutate()`:
      i In argument: `y = { ... }`.
      Caused by error:
      ! unused argument (base::quote(2))

---

    Code
      mutate(df, .by = g, y = {
        x <<- 2
        x
      })
    Condition
      Error in `mutate()`:
      i In argument: `y = { ... }`.
      i In group 1: `g = 1`.
      Caused by error:
      ! unused argument (base::quote(2))

---

    Code
      mutate(gdf, y = {
        x <<- 2
        x
      })
    Condition
      Error in `mutate()`:
      i In argument: `y = { ... }`.
      i In group 1: `g = 1`.
      Caused by error:
      ! unused argument (base::quote(2))

# can't share local variables across expressions (#6666)

    Code
      mutate(df, x2 = {
        foo <- x
        x
      }, y2 = {
        foo
      })
    Condition
      Error in `mutate()`:
      i In argument: `y2 = { ... }`.
      Caused by error:
      ! object 'foo' not found

# rowwise mutate un-lists existing size-1 list-columns (#6302)

    Code
      mutate(df, y = x)
    Condition
      Error in `mutate()`:
      i In argument: `y = x`.
      i In row 2.
      Caused by error:
      ! `y` must be size 1, not 2.
      i Did you mean: `y = list(x)` ?

# catches `.by` with grouped-df

    Code
      mutate(gdf, .by = x)
    Condition
      Error in `mutate()`:
      ! Can't supply `.by` when `.data` is a grouped data frame.

# catches `.by` with rowwise-df

    Code
      mutate(rdf, .by = x)
    Condition
      Error in `mutate()`:
      ! Can't supply `.by` when `.data` is a rowwise data frame.

# mutate() deals with 0 groups (#5534)

    Code
      mutate(df, y = max(x))
    Condition
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `y = max(x)`.
      Caused by warning in `max()`:
      ! no non-missing arguments to max; returning -Inf
    Output
      # A tibble: 0 x 2
      # Groups:   x [0]
      # i 2 variables: x <dbl>, y <dbl>

# mutate() give meaningful errors

    Code
      tbl <- tibble(x = 1:2, y = 1:2)
      (expect_error(tbl %>% mutate(y = NULL, a = sum(y))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `a = sum(y)`.
      Caused by error:
      ! object 'y' not found
    Code
      (expect_error(tbl %>% group_by(x) %>% mutate(y = NULL, a = sum(y))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `a = sum(y)`.
      i In group 1: `x = 1`.
      Caused by error:
      ! object 'y' not found
    Code
      (expect_error(tibble(x = 1) %>% mutate(y = mean)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `y = mean`.
      Caused by error:
      ! `y` must be a vector, not a function.
    Code
      df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)
      (expect_error(df %>% mutate(out = env(a = 1))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `out = env(a = 1)`.
      Caused by error:
      ! `out` must be a vector, not an environment.
    Code
      (expect_error(df %>% group_by(g) %>% mutate(out = env(a = 1))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `out = env(a = 1)`.
      i In group 1: `g = 1`.
      Caused by error:
      ! `out` must be a vector, not an environment.
    Code
      (expect_error(df %>% rowwise() %>% mutate(out = rnorm)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `out = rnorm`.
      i In row 1.
      Caused by error:
      ! `out` must be a vector, not a function.
      i Did you mean: `out = list(rnorm)` ?
    Code
      (expect_error(data.frame(x = rep(1:5, each = 3)) %>% group_by(x) %>% mutate(
        val = ifelse(x < 3, "foo", 2))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `val = ifelse(x < 3, "foo", 2)`.
      Caused by error:
      ! `val` must return compatible vectors across groups.
      i Result of type <character> for group 1: `x = 1`.
      i Result of type <double> for group 3: `x = 3`.
    Code
      (expect_error(tibble(a = 1:3, b = 4:6) %>% group_by(a) %>% mutate(if (a ==
      1) NULL else "foo")))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `if (a == 1) NULL else "foo"`.
      i In group 1: `a = 1`.
      Caused by error:
      ! `if (a == 1) NULL else "foo"` must return compatible vectors across groups.
      x Can't combine NULL and non NULL results.
    Code
      (expect_error(tibble(a = 1:3, b = 4:6) %>% group_by(a) %>% mutate(if (a ==
      2) NULL else "foo")))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `if (a == 2) NULL else "foo"`.
      i In group 2: `a = 2`.
      Caused by error:
      ! `if (a == 2) NULL else "foo"` must return compatible vectors across groups.
      x Can't combine NULL and non NULL results.
    Code
      (expect_error(data.frame(x = c(2, 2, 3, 3)) %>% mutate(int = 1:5)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `int = 1:5`.
      Caused by error:
      ! `int` must be size 4 or 1, not 5.
    Code
      (expect_error(data.frame(x = c(2, 2, 3, 3)) %>% group_by(x) %>% mutate(int = 1:
      5)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `int = 1:5`.
      i In group 1: `x = 2`.
      Caused by error:
      ! `int` must be size 2 or 1, not 5.
    Code
      (expect_error(data.frame(x = c(2, 3, 3)) %>% group_by(x) %>% mutate(int = 1:5)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `int = 1:5`.
      i In group 1: `x = 2`.
      Caused by error:
      ! `int` must be size 1, not 5.
    Code
      (expect_error(data.frame(x = c(2, 2, 3, 3)) %>% rowwise() %>% mutate(int = 1:5))
      )
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `int = 1:5`.
      i In row 1.
      Caused by error:
      ! `int` must be size 1, not 5.
      i Did you mean: `int = list(1:5)` ?
    Code
      (expect_error(tibble(y = list(1:3, "a")) %>% rowwise() %>% mutate(y2 = y)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `y2 = y`.
      i In row 1.
      Caused by error:
      ! `y2` must be size 1, not 3.
      i Did you mean: `y2 = list(y)` ?
    Code
      (expect_error(data.frame(x = 1:10) %>% mutate(y = 11:20, y = 1:2)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `y = 1:2`.
      Caused by error:
      ! `y` must be size 10 or 1, not 2.
    Code
      (expect_error(tibble(a = 1) %>% mutate(c = .data$b)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `c = .data$b`.
      Caused by error in `.data$b`:
      ! Column `b` not found in `.data`.
    Code
      (expect_error(tibble(a = 1:3) %>% group_by(a) %>% mutate(c = .data$b)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `c = .data$b`.
      i In group 1: `a = 1`.
      Caused by error in `.data$b`:
      ! Column `b` not found in `.data`.
    Code
      lazy <- (function(x) list(enquo(x)))
      res <- tbl %>% rowwise() %>% mutate(z = lazy(x), .keep = "unused")
      (expect_error(eval_tidy(res$z[[1]])))
    Output
      <error/rlang_error>
      Error:
      ! Obsolete data mask.
      x Too late to resolve `x` after the end of `dplyr::mutate()`.
      i Did you save an object that uses `x` lazily in a column in the `dplyr::mutate()` expression ?
    Code
      (expect_error(tibble() %>% mutate(stop("{"))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `stop("{")`.
      Caused by error:
      ! {

# mutate() errors refer to expressions if not named

    Code
      (expect_error(mutate(mtcars, 1:3)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `1:3`.
      Caused by error:
      ! `1:3` must be size 32 or 1, not 3.
    Code
      (expect_error(mutate(group_by(mtcars, cyl), 1:3)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `1:3`.
      i In group 1: `cyl = 4`.
      Caused by error:
      ! `1:3` must be size 11 or 1, not 3.

# `mutate()` doesn't allow data frames with missing or empty names (#6758)

    Code
      mutate(df1)
    Condition
      Error in `mutate()`:
      ! Can't transform a data frame with `NA` or `""` names.

---

    Code
      mutate(df2)
    Condition
      Error in `mutate()`:
      ! Can't transform a data frame with `NA` or `""` names.

