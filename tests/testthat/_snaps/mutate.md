# mutate() give meaningful errors

    Code
      tbl <- tibble(x = 1:2, y = 1:2)
      (expect_error(tbl %>% mutate(y = NULL, a = sum(y))))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `a`.
        i `a = sum(y)`.
        x object 'y' not found
      Caused by error: 
        object 'y' not found
    Code
      (expect_error(tbl %>% group_by(x) %>% mutate(y = NULL, a = sum(y))))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `a`.
        i `a = sum(y)`.
        x object 'y' not found
        i The error occurred in group 1: x = 1.
      Caused by error: 
        object 'y' not found
    Code
      (expect_error(tibble(x = 1) %>% mutate(y = mean)))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `y`.
        i `y = mean`.
        x `y` must be a vector, not a function.
      Caused by error in `abort_glue()`: 
        
    Code
      df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)
      (expect_error(df %>% mutate(out = env(a = 1))))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `out`.
        i `out = env(a = 1)`.
        x `out` must be a vector, not an environment.
      Caused by error in `abort_glue()`: 
        
    Code
      (expect_error(df %>% group_by(g) %>% mutate(out = env(a = 1))))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `out`.
        i `out = env(a = 1)`.
        x `out` must be a vector, not an environment.
        i The error occurred in group 1: g = 1.
      Caused by error in `abort_glue()`: 
        
    Code
      (expect_error(df %>% rowwise() %>% mutate(out = rnorm)))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `out`.
        i `out = rnorm`.
        x `out` must be a vector, not a function.
        i Did you mean: `out = list(rnorm)` ?
        i The error occurred in row 1.
      Caused by error in `abort_glue()`: 
        
    Code
      (expect_error(data.frame(x = rep(1:5, each = 3)) %>% group_by(x) %>% mutate(
        val = ifelse(x < 3, "foo", 2))))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `val`.
        i `val = ifelse(x < 3, "foo", 2)`.
        x `val` must return compatible vectors across groups
        i Result type for group 1 (x = 1): <character>.
        i Result type for group 3 (x = 3): <double>.
      Caused by error: 
        
      Caused by error in `stop_vctrs()`: 
        Can't combine `..1` <character> and `..3` <double>.
    Code
      (expect_error(tibble(a = 1:3, b = 4:6) %>% group_by(a) %>% mutate(if (a ==
      1) NULL else "foo")))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` input `..1`.
        i `..1 = if (a == 1) NULL else "foo"`.
        x `..1` must return compatible vectors across groups.
        i Cannot combine NULL and non NULL results.
      Caused by error in `abort_glue()`: 
        
    Code
      (expect_error(data.frame(x = c(2, 2, 3, 3)) %>% mutate(int = 1:5)))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `int`.
        i `int = 1:5`.
        i `int` must be size 4 or 1, not 5.
      Caused by error in `abort_glue()`: 
        
    Code
      (expect_error(data.frame(x = c(2, 2, 3, 3)) %>% group_by(x) %>% mutate(int = 1:
      5)))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `int`.
        i `int = 1:5`.
        i `int` must be size 2 or 1, not 5.
        i The error occurred in group 1: x = 2.
      Caused by error in `abort_glue()`: 
        
    Code
      (expect_error(data.frame(x = c(2, 3, 3)) %>% group_by(x) %>% mutate(int = 1:5)))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `int`.
        i `int = 1:5`.
        i `int` must be size 1, not 5.
        i The error occurred in group 1: x = 2.
      Caused by error in `abort_glue()`: 
        
    Code
      (expect_error(data.frame(x = c(2, 2, 3, 3)) %>% rowwise() %>% mutate(int = 1:5))
      )
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `int`.
        i `int = 1:5`.
        i `int` must be size 1, not 5.
        i Did you mean: `int = list(1:5)` ?
        i The error occurred in row 1.
      Caused by error in `abort_glue()`: 
        
    Code
      (expect_error(tibble(y = list(1:3, "a")) %>% rowwise() %>% mutate(y2 = y)))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `y2`.
        i `y2 = y`.
        i `y2` must be size 1, not 3.
        i Did you mean: `y2 = list(y)` ?
        i The error occurred in row 1.
      Caused by error in `mutate_cols()`: 
        
    Code
      (expect_error(data.frame(x = 1:10) %>% mutate(y = 11:20, y = 1:2)))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `y`.
        i `y = 1:2`.
        i `y` must be size 10 or 1, not 2.
      Caused by error in `abort_glue()`: 
        
    Code
      (expect_error(tibble(a = 1) %>% mutate(c = .data$b)))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `c`.
        i `c = .data$b`.
        x Column `b` not found in `.data`.
      Caused by error in `.data$b`: 
        Column `b` not found in `.data`.
    Code
      (expect_error(tibble(a = 1:3) %>% group_by(a) %>% mutate(c = .data$b)))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` column `c`.
        i `c = .data$b`.
        x Column `b` not found in `.data`.
        i The error occurred in group 1: a = 1.
      Caused by error in `.data$b`: 
        Column `b` not found in `.data`.
    Code
      lazy <- (function(x) list(enquo(x)))
      res <- tbl %>% rowwise() %>% mutate(z = lazy(x), .keep = "unused")
      (expect_error(eval_tidy(res$z[[1]])))
    Output
      <error/rlang_error>
      Error in `osbolete_promise_fn()`: Obsolete data mask.
      x Too late to resolve `x` after the end of `dplyr::mutate()`.
      i Did you save an object that uses `x` lazily in a column in the `dplyr::mutate()` expression ?
    Code
      (expect_error(tibble() %>% mutate(stop("{"))))
    Output
      <error/dplyr:::mutate_error>
      Error: 
        Problem with `mutate()` input `..1`.
        i `..1 = stop("{")`.
        x {
      Caused by error: 
        {

