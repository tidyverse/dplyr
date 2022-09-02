# mutate() supports constants (#6056, #6305)

    Code
      (expect_error(df %>% mutate(z = !!z)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `z = <int>`.
      Inlined constant `z` must be size 10 or 1, not 5.
    Code
      (expect_error(df %>% group_by(g) %>% mutate(z = !!z)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `z = <int>`.
      Inlined constant `z` must be size 10 or 1, not 5.
    Code
      (expect_error(df %>% rowwise() %>% mutate(z = !!z)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `z = <int>`.
      Inlined constant `z` must be size 10 or 1, not 5.

---

    Code
      (expect_error(df %>% group_by(g) %>% mutate(y = .env$y)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `y = .env$y`.
      x `y` must be size 5 or 1, not 10.
      i The error occurred in group 1: g = 1.
    Code
      (expect_error(df %>% rowwise() %>% mutate(y = .env$y)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `y = .env$y`.
      x `y` must be size 1, not 10.
      i Did you mean: `y = list(.env$y)` ?
      i The error occurred in row 1.

# rowwise mutate un-lists existing size-1 list-columns (#6302)

    Code
      mutate(df, y = x)
    Condition
      Error in `mutate()`:
      ! Problem while computing `y = x`.
      x `y` must be size 1, not 2.
      i Did you mean: `y = list(x)` ?
      i The error occurred in row 2.

# mutate() deals with 0 groups (#5534)

    Code
      mutate(df, y = max(x))
    Condition
      Warning:
      There was 1 warning in a `mutate()` step.
      ! no non-missing arguments to max; returning -Inf
    Output
      # A tibble: 0 x 2
      # Groups:   x [0]
      # ... with 2 variables: x <dbl>, y <dbl>

# mutate() give meaningful errors

    Code
      tbl <- tibble(x = 1:2, y = 1:2)
      (expect_error(tbl %>% mutate(y = NULL, a = sum(y))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `a = sum(y)`.
      Caused by error:
      ! object 'y' not found
    Code
      (expect_error(tbl %>% group_by(x) %>% mutate(y = NULL, a = sum(y))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `a = sum(y)`.
      i The error occurred in group 1: x = 1.
      Caused by error:
      ! object 'y' not found
    Code
      (expect_error(tibble(x = 1) %>% mutate(y = mean)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `y = mean`.
      x `y` must be a vector, not a function.
    Code
      df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)
      (expect_error(df %>% mutate(out = env(a = 1))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `out = env(a = 1)`.
      x `out` must be a vector, not an environment.
    Code
      (expect_error(df %>% group_by(g) %>% mutate(out = env(a = 1))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `out = env(a = 1)`.
      x `out` must be a vector, not an environment.
      i The error occurred in group 1: g = 1.
    Code
      (expect_error(df %>% rowwise() %>% mutate(out = rnorm)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `out = rnorm`.
      x `out` must be a vector, not a function.
      i Did you mean: `out = list(rnorm)` ?
      i The error occurred in row 1.
    Code
      (expect_error(data.frame(x = rep(1:5, each = 3)) %>% group_by(x) %>% mutate(
        val = ifelse(x < 3, "foo", 2))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `val = ifelse(x < 3, "foo", 2)`.
      Caused by error:
      ! `val` must return compatible vectors across groups.
      i Result type for group 1 (x = 1): <character>.
      i Result type for group 3 (x = 3): <double>.
    Code
      (expect_error(tibble(a = 1:3, b = 4:6) %>% group_by(a) %>% mutate(if (a ==
      1) NULL else "foo")))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `..1 = if (a == 1) NULL else "foo"`.
      x `..1` must return compatible vectors across groups.
      x Can't combine NULL and non NULL results.
    Code
      (expect_error(data.frame(x = c(2, 2, 3, 3)) %>% mutate(int = 1:5)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `int = 1:5`.
      x `int` must be size 4 or 1, not 5.
    Code
      (expect_error(data.frame(x = c(2, 2, 3, 3)) %>% group_by(x) %>% mutate(int = 1:
      5)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `int = 1:5`.
      x `int` must be size 2 or 1, not 5.
      i The error occurred in group 1: x = 2.
    Code
      (expect_error(data.frame(x = c(2, 3, 3)) %>% group_by(x) %>% mutate(int = 1:5)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `int = 1:5`.
      x `int` must be size 1, not 5.
      i The error occurred in group 1: x = 2.
    Code
      (expect_error(data.frame(x = c(2, 2, 3, 3)) %>% rowwise() %>% mutate(int = 1:5))
      )
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `int = 1:5`.
      x `int` must be size 1, not 5.
      i Did you mean: `int = list(1:5)` ?
      i The error occurred in row 1.
    Code
      (expect_error(tibble(y = list(1:3, "a")) %>% rowwise() %>% mutate(y2 = y)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `y2 = y`.
      x `y2` must be size 1, not 3.
      i Did you mean: `y2 = list(y)` ?
      i The error occurred in row 1.
    Code
      (expect_error(data.frame(x = 1:10) %>% mutate(y = 11:20, y = 1:2)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `y = 1:2`.
      x `y` must be size 10 or 1, not 2.
    Code
      (expect_error(tibble(a = 1) %>% mutate(c = .data$b)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `c = .data$b`.
      Caused by error in `.data$b`:
      ! Column `b` not found in `.data`.
    Code
      (expect_error(tibble(a = 1:3) %>% group_by(a) %>% mutate(c = .data$b)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `c = .data$b`.
      i The error occurred in group 1: a = 1.
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
      ! Problem while computing `..1 = stop("{")`.
      Caused by error:
      ! {

