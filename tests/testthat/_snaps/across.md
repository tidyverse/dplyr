# across() gives meaningful messages

    Code
      (expect_error(tibble(x = 1) %>% summarise(across(where(is.numeric), 42))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `..1 = across(where(is.numeric), 42)`.
      Caused by error in `across()`:
      ! `.fns` must be NULL, a function, a formula, or a list of functions/formulas.
    Code
      (expect_error(tibble(x = 1) %>% summarise(across(y, mean))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `..1 = across(y, mean)`.
      Caused by error in `across()`:
      ! Can't subset columns that don't exist.
      x Column `y` doesn't exist.
    Code
      (expect_error(tibble(x = 1) %>% summarise(res = across(where(is.numeric), 42))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `res = across(where(is.numeric), 42)`.
      Caused by error in `across()`:
      ! `.fns` must be NULL, a function, a formula, or a list of functions/formulas.
    Code
      (expect_error(tibble(x = 1) %>% summarise(z = across(y, mean))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `z = across(y, mean)`.
      Caused by error in `across()`:
      ! Can't subset columns that don't exist.
      x Column `y` doesn't exist.
    Code
      (expect_error(tibble(x = 1) %>% summarise(res = sum(if_any(where(is.numeric),
      42)))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `res = sum(if_any(where(is.numeric), 42))`.
      Caused by error in `if_any()`:
      ! `.fns` must be NULL, a function, a formula, or a list of functions/formulas.
    Code
      (expect_error(tibble(x = 1) %>% summarise(res = sum(if_all(~ mean(.x))))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `res = sum(if_all(~mean(.x)))`.
      Caused by error in `if_all()`:
      ! Must supply a column selection.
      i You most likely meant: `if_all(everything(), ~mean(.x))`.
      i The first argument `.cols` selects a set of columns.
      i The second argument `.fns` operates on each selected columns.
    Code
      (expect_error(tibble(x = 1) %>% summarise(res = sum(if_any(~ mean(.x))))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `res = sum(if_any(~mean(.x)))`.
      Caused by error in `if_any()`:
      ! Must supply a column selection.
      i You most likely meant: `if_any(everything(), ~mean(.x))`.
      i The first argument `.cols` selects a set of columns.
      i The second argument `.fns` operates on each selected columns.
    Code
      (expect_error(across()))
    Output
      <error/rlang_error>
      Error in `across()`:
      ! Must be used inside dplyr verbs.
    Code
      (expect_error(c_across()))
    Output
      <error/rlang_error>
      Error in `c_across()`:
      ! Must be used inside dplyr verbs.
    Code
      error_fn <- (function(.) {
        if (all(. > 10)) {
          rlang::abort("too small", call = call("error_fn"))
        } else {
          42
        }
      })
      (expect_error(tibble(x = 1:10, y = 11:20) %>% summarise(across(everything(),
      error_fn))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `..1 = across(everything(), error_fn)`.
      Caused by error in `across()`:
      ! Problem while computing column `y`.
      Caused by error in `error_fn()`:
      ! too small
    Code
      (expect_error(tibble(x = 1:10, y = 11:20) %>% mutate(across(everything(),
      error_fn))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `..1 = across(everything(), error_fn)`.
      Caused by error in `across()`:
      ! Problem while computing column `y`.
      Caused by error in `error_fn()`:
      ! too small
    Code
      (expect_error(tibble(x = 1:10, y = 11:20) %>% summarise(force(across(everything(),
      error_fn)))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `..1 = force(across(everything(), error_fn))`.
      Caused by error in `across()`:
      ! Problem while computing column `y`.
      Caused by error in `error_fn()`:
      ! too small
    Code
      (expect_error(tibble(x = 1:10, y = 11:20) %>% mutate(force(across(everything(),
      error_fn)))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `..1 = force(across(everything(), error_fn))`.
      Caused by error in `across()`:
      ! Problem while computing column `y`.
      Caused by error in `error_fn()`:
      ! too small
    Code
      (expect_error(tibble(x = 1) %>% summarise(across(everything(), list(f = mean,
        f = mean)))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Problem while computing `..1 = across(everything(), list(f = mean, f = mean))`.
      Caused by error in `across()`:
      ! Names must be unique.
      x These names are duplicated:
        * "x_f" at locations 1 and 2.

# if_any() and if_all() aborts when predicate mistakingly used in .cols= (#5732)

    Code
      (expect_error(filter(df, if_any(~ .x > 5))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while expanding `..1 = if_any(~.x > 5)`.
      Caused by error in `if_any()`:
      ! Must supply a column selection.
      i You most likely meant: `if_any(everything(), ~.x > 5)`.
      i The first argument `.cols` selects a set of columns.
      i The second argument `.fns` operates on each selected columns.
    Code
      (expect_error(filter(df, if_all(~ .x > 5))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while expanding `..1 = if_all(~.x > 5)`.
      Caused by error in `if_all()`:
      ! Must supply a column selection.
      i You most likely meant: `if_all(everything(), ~.x > 5)`.
      i The first argument `.cols` selects a set of columns.
      i The second argument `.fns` operates on each selected columns.
    Code
      (expect_error(filter(df, !if_any(~ .x > 5))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = !if_any(~.x > 5)`.
      Caused by error in `if_any()`:
      ! Must supply a column selection.
      i You most likely meant: `if_any(everything(), ~.x > 5)`.
      i The first argument `.cols` selects a set of columns.
      i The second argument `.fns` operates on each selected columns.
    Code
      (expect_error(filter(df, !if_all(~ .x > 5))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = !if_all(~.x > 5)`.
      Caused by error in `if_all()`:
      ! Must supply a column selection.
      i You most likely meant: `if_all(everything(), ~.x > 5)`.
      i The first argument `.cols` selects a set of columns.
      i The second argument `.fns` operates on each selected columns.

