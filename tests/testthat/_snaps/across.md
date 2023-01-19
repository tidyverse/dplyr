# across(.unpack =) errors if the unpacked data frame has non-unique names

    Code
      mutate(df, across(x:y, fn, .unpack = "{outer}"))
    Condition
      Error in `mutate()`:
      i In argument: `across(x:y, fn, .unpack = "{outer}")`.
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
      i In argument: `across(x, mean, .unpack = 1)`.
      Caused by error in `across()`:
      ! `.unpack` must be `TRUE`, `FALSE`, or a single string, not the number 1.

---

    Code
      summarise(df, across(x, mean, .unpack = c("x", "y")))
    Condition
      Error in `summarise()`:
      i In argument: `across(x, mean, .unpack = c("x", "y"))`.
      Caused by error in `across()`:
      ! `.unpack` must be `TRUE`, `FALSE`, or a single string, not a character vector.

---

    Code
      summarise(df, across(x, mean, .unpack = NA))
    Condition
      Error in `summarise()`:
      i In argument: `across(x, mean, .unpack = NA)`.
      Caused by error in `across()`:
      ! `.unpack` must be `TRUE`, `FALSE`, or a single string, not `NA`.

# across() throws meaningful error with failure during expansion (#6534)

    Code
      summarise(df, across(everything(), median()))
    Condition
      Error in `summarise()`:
      i In argument: `across(everything(), median())`.
      Caused by error in `is.factor()`:
      ! argument "x" is missing, with no default

---

    Code
      summarise(df, across(everything(), median()), .by = g)
    Condition
      Error in `summarise()`:
      i In argument: `across(everything(), median())`.
      Caused by error in `is.factor()`:
      ! argument "x" is missing, with no default

---

    Code
      summarise(gdf, across(everything(), median()))
    Condition
      Error in `summarise()`:
      i In argument: `across(everything(), median())`.
      Caused by error in `is.factor()`:
      ! argument "x" is missing, with no default

# across() gives meaningful messages

    Code
      (expect_error(tibble(x = 1) %>% summarise(across(where(is.numeric), 42))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `across(where(is.numeric), 42)`.
      Caused by error in `across()`:
      ! `.fns` must be a function, a formula, or a list of functions/formulas.
    Code
      (expect_error(tibble(x = 1) %>% summarise(across(y, mean))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `across(y, mean)`.
      Caused by error in `across()`:
      ! Can't subset columns that don't exist.
      x Column `y` doesn't exist.
    Code
      (expect_error(tibble(x = 1) %>% summarise(res = across(where(is.numeric), 42))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `res = across(where(is.numeric), 42)`.
      Caused by error in `across()`:
      ! `.fns` must be a function, a formula, or a list of functions/formulas.
    Code
      (expect_error(tibble(x = 1) %>% summarise(z = across(y, mean))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `z = across(y, mean)`.
      Caused by error in `across()`:
      ! Can't subset columns that don't exist.
      x Column `y` doesn't exist.
    Code
      (expect_error(tibble(x = 1) %>% summarise(res = sum(if_any(where(is.numeric),
      42)))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `res = sum(if_any(where(is.numeric), 42))`.
      Caused by error in `if_any()`:
      ! `.fns` must be a function, a formula, or a list of functions/formulas.
    Code
      (expect_error(tibble(x = 1) %>% summarise(res = sum(if_all(~ mean(.x))))))
    Output
      <error/rlang_error>
      Error in `summarise()`:
      i In argument: `res = sum(if_all(~mean(.x)))`.
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
      i In argument: `res = sum(if_any(~mean(.x)))`.
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
      i In argument: `across(everything(), error_fn)`.
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
      i In argument: `across(everything(), error_fn)`.
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
      i In argument: `force(across(everything(), error_fn))`.
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
      i In argument: `force(across(everything(), error_fn))`.
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
      i In argument: `across(everything(), list(f = mean, f = mean))`.
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
      i In argument: `if_any(~.x > 5)`.
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
      i In argument: `if_all(~.x > 5)`.
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
      i In argument: `!if_any(~.x > 5)`.
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
      i In argument: `!if_all(~.x > 5)`.
      Caused by error in `if_all()`:
      ! Must supply a column selection.
      i You most likely meant: `if_all(everything(), ~.x > 5)`.
      i The first argument `.cols` selects a set of columns.
      i The second argument `.fns` operates on each selected columns.

# inlined and non inlined lambdas work

    Code
      (expect_error(df %>% mutate(across(1:2, ~ .y + mean(bar)))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `across(1:2, ~.y + mean(bar))`.
      Caused by error in `across()`:
      ! Can't compute column `foo`.
      Caused by error:
      ! the ... list contains fewer than 2 elements
    Code
      (expect_error(df %>% mutate((across(1:2, ~ .y + mean(bar))))))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      i In argument: `(across(1:2, ~.y + mean(bar)))`.
      Caused by error in `across()`:
      ! Can't compute column `foo`.
      Caused by error in `fn()`:
      ! the ... list contains fewer than 2 elements

# can't rename during selection (#6522)

    Code
      mutate(df, z = c_across(c(y = x)))
    Condition
      Error in `mutate()`:
      i In argument: `z = c_across(c(y = x))`.
      Caused by error in `c_across()`:
      ! Can't rename variables in this context.

# can't explicitly select grouping columns (#6522)

    Code
      mutate(gdf, y = c_across(g))
    Condition
      Error in `mutate()`:
      i In argument: `y = c_across(g)`.
      i In group 1: `g = 1`.
      Caused by error in `c_across()`:
      ! Can't subset columns that don't exist.
      x Column `g` doesn't exist.

# `all_of()` is evaluated in the correct environment (#6522)

    Code
      mutate(df, z = c_across(all_of(y)))
    Condition
      Error in `mutate()`:
      i In argument: `z = c_across(all_of(y))`.
      Caused by error in `c_across()`:
      ! Problem while evaluating `all_of(y)`.
      Caused by error in `as_indices_impl()`:
      ! object 'y' not found

# across() applies old `.cols = everything()` default with a warning

    Code
      out <- mutate(df, across(.fns = times_two))
    Condition
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `across(.fns = times_two)`.
      Caused by warning:
      ! Using `across()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.

---

    Code
      out <- mutate(gdf, across(.fns = times_two))
    Condition
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `across(.fns = times_two)`.
      Caused by warning:
      ! Using `across()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.

---

    Code
      out <- mutate(df, (across(.fns = times_two)))
    Condition
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `(across(.fns = times_two))`.
      Caused by warning:
      ! Using `across()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.

---

    Code
      out <- mutate(gdf, (across(.fns = times_two)))
    Condition
      Warning:
      There were 2 warnings in `mutate()`.
      The first warning was:
      i In argument: `(across(.fns = times_two))`.
      i In group 1: `g = 1`.
      Caused by warning:
      ! Using `across()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

# if_any() and if_all() apply old `.cols = everything()` default with a warning

    Code
      out <- filter(df, if_any())
    Condition
      Warning:
      Using `if_any()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.

---

    Code
      out <- filter(gdf, if_any())
    Condition
      Warning:
      Using `if_any()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.

---

    Code
      out <- filter(df, if_all())
    Condition
      Warning:
      Using `if_all()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.

---

    Code
      out <- filter(gdf, if_all())
    Condition
      Warning:
      Using `if_all()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.

---

    Code
      out <- filter(df, (if_any()))
    Condition
      Warning:
      There was 1 warning in `filter()`.
      i In argument: `(if_any())`.
      Caused by warning:
      ! Using `if_any()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.

---

    Code
      out <- filter(gdf, (if_any()))
    Condition
      Warning:
      There were 2 warnings in `filter()`.
      The first warning was:
      i In argument: `(if_any())`.
      i In group 1: `g = 1`.
      Caused by warning:
      ! Using `if_any()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

---

    Code
      out <- filter(df, (if_all()))
    Condition
      Warning:
      There was 1 warning in `filter()`.
      i In argument: `(if_all())`.
      Caused by warning:
      ! Using `if_all()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.

---

    Code
      out <- filter(gdf, (if_all()))
    Condition
      Warning:
      There were 2 warnings in `filter()`.
      The first warning was:
      i In argument: `(if_all())`.
      i In group 1: `g = 1`.
      Caused by warning:
      ! Using `if_all()` without supplying `.cols` was deprecated in dplyr 1.1.0.
      i Please supply `.cols` instead.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

# c_across() applies old `cols = everything()` default with a warning

    Code
      out <- mutate(df, z = sum(c_across()))
    Condition
      Warning:
      There were 2 warnings in `mutate()`.
      The first warning was:
      i In argument: `z = sum(c_across())`.
      i In row 1.
      Caused by warning:
      ! Using `c_across()` without supplying `cols` was deprecated in dplyr 1.1.0.
      i Please supply `cols` instead.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.

# across errors with non-empty dots and no `.fns` supplied (#6638)

    Code
      mutate(df, across(x, .funs = ~ . * 1000))
    Condition
      Error in `mutate()`:
      i In argument: `across(x, .funs = ~. * 1000)`.
      Caused by error in `across()`:
      ! `...` must be empty.
      x Problematic argument:
      * .funs = ~. * 1000

# across(...) is deprecated

    Code
      summarise(df, across(everything(), mean, na.rm = TRUE))
    Condition
      Warning:
      There was 1 warning in `summarise()`.
      i In argument: `across(everything(), mean, na.rm = TRUE)`.
      Caused by warning:
      ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
      Supply arguments directly to `.fns` through an anonymous function instead.
      
        # Previously
        across(a:b, mean, na.rm = TRUE)
      
        # Now
        across(a:b, \(x) mean(x, na.rm = TRUE))
    Output
      # A tibble: 1 x 1
            x
        <dbl>
      1     1

