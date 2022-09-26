# across(.unpack =) errors if the unpacked data frame has non-unique names

    Code
      mutate(df, across(x:y, fn, .unpack = "{outer}"))
    Condition
      Error in `mutate()`:
      ! Can't compute `..1 = across(x:y, fn, .unpack = "{outer}")`.
      Caused by error in `across()`:
      ! Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.
        * "y" at locations 3 and 4.

# `.unpack` is validated

    Code
      summarise(df, across(x, mean, .unpack = 1))
    Condition
      Error in `summarise()`:
      ! Can't compute `..1 = across(x, mean, .unpack = 1)`.
      Caused by error in `across()`:
      ! `.unpack` must be `TRUE`, `FALSE`, or a single string, not a number.

---

    Code
      summarise(df, across(x, mean, .unpack = c("x", "y")))
    Condition
      Error in `summarise()`:
      ! Can't compute `..1 = across(x, mean, .unpack = c("x", "y"))`.
      Caused by error in `across()`:
      ! `.unpack` must be `TRUE`, `FALSE`, or a single string, not a character vector.

---

    Code
      summarise(df, across(x, mean, .unpack = NA))
    Condition
      Error in `summarise()`:
      ! Can't compute `..1 = across(x, mean, .unpack = NA)`.
      Caused by error in `across()`:
      ! `.unpack` must be `TRUE`, `FALSE`, or a single string, not `NA`.

# across() gives meaningful messages

    Code
      (expect_error(tibble(x = 1) %>% summarise(across(where(is.numeric), 42))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't compute `..1 = across(where(is.numeric), 42)`.
      Caused by error in `across()`:
      ! `.fns` must be NULL, a function, a formula, or a list of functions/formulas.
    Code
      (expect_error(tibble(x = 1) %>% summarise(across(y, mean))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't compute `..1 = across(y, mean)`.
      Caused by error in `across()`:
      ! Can't subset columns that don't exist.
      x Column `y` doesn't exist.
    Code
      (expect_error(tibble(x = 1) %>% summarise(res = across(where(is.numeric), 42))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't compute `res = across(where(is.numeric), 42)`.
      Caused by error in `across()`:
      ! `.fns` must be NULL, a function, a formula, or a list of functions/formulas.
    Code
      (expect_error(tibble(x = 1) %>% summarise(z = across(y, mean))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't compute `z = across(y, mean)`.
      Caused by error in `across()`:
      ! Can't subset columns that don't exist.
      x Column `y` doesn't exist.
    Code
      (expect_error(tibble(x = 1) %>% summarise(res = sum(if_any(where(is.numeric),
      42)))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't compute `res = sum(if_any(where(is.numeric), 42))`.
      Caused by error in `if_any()`:
      ! `.fns` must be NULL, a function, a formula, or a list of functions/formulas.
    Code
      (expect_error(tibble(x = 1) %>% summarise(res = sum(if_all(~ mean(.x))))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't compute `res = sum(if_all(~mean(.x)))`.
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
      ! Can't compute `res = sum(if_any(~mean(.x)))`.
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
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.
    Code
      (expect_error(c_across()))
    Output
      <error/rlang_error>
      Error in `c_across()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.
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
      ! Can't compute `..1 = across(everything(), error_fn)`.
      Caused by error in `across()`:
      ! Can't compute column `y`.
      Caused by error in `error_fn()`:
      ! too small
    Code
      (expect_error(tibble(x = 1:10, y = 11:20) %>% mutate(across(everything(),
      error_fn))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Can't compute `..1 = across(everything(), error_fn)`.
      Caused by error in `across()`:
      ! Can't compute column `y`.
      Caused by error in `error_fn()`:
      ! too small
    Code
      (expect_error(tibble(x = 1:10, y = 11:20) %>% summarise(force(across(everything(),
      error_fn)))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't compute `..1 = force(across(everything(), error_fn))`.
      Caused by error in `across()`:
      ! Can't compute column `y`.
      Caused by error in `error_fn()`:
      ! too small
    Code
      (expect_error(tibble(x = 1:10, y = 11:20) %>% mutate(force(across(everything(),
      error_fn)))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Can't compute `..1 = force(across(everything(), error_fn))`.
      Caused by error in `across()`:
      ! Can't compute column `y`.
      Caused by error in `error_fn()`:
      ! too small
    Code
      (expect_error(tibble(x = 1) %>% summarise(across(everything(), list(f = mean,
        f = mean)))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      ! Can't compute `..1 = across(everything(), list(f = mean, f = mean))`.
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
      ! Can't expand `..1 = if_any(~.x > 5)`.
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
      ! Can't expand `..1 = if_all(~.x > 5)`.
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
      ! Can't compute `..1 = !if_any(~.x > 5)`.
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
      ! Can't compute `..1 = !if_all(~.x > 5)`.
      Caused by error in `if_all()`:
      ! Must supply a column selection.
      i You most likely meant: `if_all(everything(), ~.x > 5)`.
      i The first argument `.cols` selects a set of columns.
      i The second argument `.fns` operates on each selected columns.

# across(...) is deprecated

    Code
      summarise(df, across(everything(), mean, na.rm = TRUE))
    Condition
      Warning:
      There was 1 warning in `summarise()`.
      i In argument `..1 = across(everything(), mean, na.rm = TRUE)`.
      Caused by warning:
      ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
      Supply arguments directly to `.fns` through a lambda instead.
      
        # Previously
        across(a:b, mean, na.rm = TRUE)
      
        # Now
        across(a:b, ~mean(.x, na.rm = TRUE))
    Output
      # A tibble: 1 x 1
            x
        <dbl>
      1     1

