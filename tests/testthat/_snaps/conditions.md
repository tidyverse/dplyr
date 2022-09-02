# can hide expression in error messages

    Code
      mutate(mtcars, invisible(999 + ""))
    Condition
      Error in `mutate()`:
      ! Problem while computing `..1`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      summarise(mtcars, invisible(999 + ""))
    Condition
      Error in `summarise()`:
      ! Problem while computing `..1`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      filter(mtcars, invisible(999 + ""))
    Condition
      Error in `filter()`:
      ! Problem while computing `..1`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      arrange(mtcars, invisible(999 + ""))
    Condition
      Error in `arrange()`:
      ! Problem while computing `..1`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      select(mtcars, invisible(999 + ""))
    Condition
      Error in `select()`:
      ! Problem while evaluating `invisible(999 + "")`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      slice(mtcars, invisible(999 + ""))
    Condition
      Error in `slice()`:
      ! Problem while evaluating `..1`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      mutate(mtcars, var = invisible(999 + ""))
    Condition
      Error in `mutate()`:
      ! Problem while computing `var`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      summarise(mtcars, var = invisible(999 + ""))
    Condition
      Error in `summarise()`:
      ! Problem while computing `var`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      filter(mtcars, var = invisible(999 + ""))
    Condition
      Error in `filter()`:
      ! We detected a named input.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `var == invisible(999 + "")`?
    Code
      arrange(mtcars, var = invisible(999 + ""))
    Condition
      Error in `arrange()`:
      ! Problem while computing `..1`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      select(mtcars, var = invisible(999 + ""))
    Condition
      Error in `select()`:
      ! Problem while evaluating `invisible(999 + "")`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      slice(mtcars, var = invisible(999 + ""))
    Condition
      Error in `slice()`:
      ! Problem while evaluating `var`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator

# can pass verb-level error call

    Code
      mutate(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      ! Problem while computing `..1 = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      transmute(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      ! Problem while computing `..1 = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      summarise(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      ! Problem while computing `..1 = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      summarise(group_by(mtcars, cyl), 1 + "")
    Condition
      Error in `foo()`:
      ! Problem while computing `..1 = 1 + ""`.
      i The error occurred in group 1: cyl = 4.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      filter(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      ! Problem while computing `..1 = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      arrange(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      ! Problem while computing `..1 = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      select(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      ! Problem while evaluating `1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      slice(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      ! Problem while evaluating `..1 = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

# can pass verb-level error call (example case)

    Code
      my_verb(mtcars, 1 + "", am)
    Condition
      Error in `my_verb()`:
      ! Problem while computing `.result = (1 + "") * am`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      my_verb(mtcars, cyl, c(am, vs))
    Condition
      Error in `my_verb()`:
      ! Problem while computing `.result = cyl * c(am, vs)`.
      x `.result` must be size 32 or 1, not 64.

# `err_locs()` works as expected

    Code
      err_locs(1.5)
    Condition
      Error in `err_locs()`:
      ! `x` must be an integer vector of locations.
      i This is an internal error in the dplyr package, please report it to the package authors.

---

    Code
      err_locs(integer())
    Condition
      Error in `err_locs()`:
      ! `x` must have at least 1 location.
      i This is an internal error in the dplyr package, please report it to the package authors.

---

    Code
      err_locs(1L)
    Output
      `c(1)`
    Code
      err_locs(1:5)
    Output
      `c(1, 2, 3, 4, 5)`
    Code
      err_locs(1:6)
    Output
      `c(1, 2, 3, 4, 5)` and 1 more
    Code
      err_locs(1:7)
    Output
      `c(1, 2, 3, 4, 5)` and 2 more

# errors during dots collection are not enriched (#6178)

    Code
      mutate(mtcars, !!foobarbaz())
    Condition
      Error in `foobarbaz()`:
      ! could not find function "foobarbaz"
    Code
      transmute(mtcars, !!foobarbaz())
    Condition
      Error in `foobarbaz()`:
      ! could not find function "foobarbaz"
    Code
      select(mtcars, !!foobarbaz())
    Condition
      Error in `foobarbaz()`:
      ! could not find function "foobarbaz"
    Code
      arrange(mtcars, !!foobarbaz())
    Condition
      Error in `foobarbaz()`:
      ! could not find function "foobarbaz"
    Code
      filter(mtcars, !!foobarbaz())
    Condition
      Error in `foobarbaz()`:
      ! could not find function "foobarbaz"

# warnings are collected for `dplyr_last_warnings()`

    Code
      # Ungrouped
      invisible(mutate(df, x = f()))
    Condition
      Warning:
      There was 1 warning in a `mutate()` step.
      ! msg
    Code
      dplyr_last_warnings()
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(df, x = f())
        2. +-dplyr:::mutate.data.frame(df, x = f())
        3. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        4. |   +-base::withCallingHandlers(...)
        5. |   \-(function() {...
        6. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        7. |       \-mask$eval_all_mutate(quo)
        8. |         \-dplyr (local) eval()
        9. \-dplyr (local) f()
       10.   \-base::warning("msg")
      

---

    Code
      # Grouped
      invisible(mutate(group_by(df, id), x = f()))
    Condition
      Warning:
      There were 2 warnings in a `mutate()` step.
      The first warning was:
      ! msg
      i Run `dplyr::dplyr_last_warnings()` to see the 1 remaining warning.
    Code
      dplyr_last_warnings()
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in group 1: id = 1 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(group_by(df, id), x = f())
        2. +-dplyr:::mutate.data.frame(group_by(df, id), x = f())
        3. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        4. |   +-base::withCallingHandlers(...)
        5. |   \-(function() {...
        6. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        7. |       \-mask$eval_all_mutate(quo)
        8. |         \-dplyr (local) eval()
        9. \-dplyr (local) f()
       10.   \-base::warning("msg")
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in group 2: id = 2 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(group_by(df, id), x = f())
        2. +-dplyr:::mutate.data.frame(group_by(df, id), x = f())
        3. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        4. |   +-base::withCallingHandlers(...)
        5. |   \-(function() {...
        6. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        7. |       \-mask$eval_all_mutate(quo)
        8. |         \-dplyr (local) eval()
        9. \-dplyr (local) f()
       10.   \-base::warning("msg")
      

---

    Code
      # Rowwise
      invisible(mutate(rowwise(df), x = f()))
    Condition
      Warning:
      There were 2 warnings in a `mutate()` step.
      The first warning was:
      ! msg
      i Run `dplyr::dplyr_last_warnings()` to see the 1 remaining warning.
    Code
      dplyr_last_warnings()
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in row 1 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(rowwise(df), x = f())
        2. +-dplyr:::mutate.data.frame(rowwise(df), x = f())
        3. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        4. |   +-base::withCallingHandlers(...)
        5. |   \-(function() {...
        6. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        7. |       \-mask$eval_all_mutate(quo)
        8. |         \-dplyr (local) eval()
        9. \-dplyr (local) f()
       10.   \-base::warning("msg")
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in row 2 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(rowwise(df), x = f())
        2. +-dplyr:::mutate.data.frame(rowwise(df), x = f())
        3. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        4. |   +-base::withCallingHandlers(...)
        5. |   \-(function() {...
        6. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        7. |       \-mask$eval_all_mutate(quo)
        8. |         \-dplyr (local) eval()
        9. \-dplyr (local) f()
       10.   \-base::warning("msg")
      

---

    Code
      # Multiple type of warnings within multiple verbs
      invisible(mutate(group_by(mutate(rowwise(group_by(df, g = f():n())), x = f()),
      id), x = f()))
    Condition
      Warning:
      There was 1 warning in a `group_by()` step.
      ! msg
      Warning:
      There were 2 warnings in a `mutate()` step.
      The first warning was:
      ! msg
      i Run `dplyr::dplyr_last_warnings()` to see the 1 remaining warning.
      Warning:
      There were 2 warnings in a `mutate()` step.
      The first warning was:
      ! msg
      i Run `dplyr::dplyr_last_warnings()` to see the 1 remaining warning.
    Code
      dplyr_last_warnings()
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `group_by()`:
      Problem while computing `g = f():n()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(...)
        2. +-dplyr::group_by(...)
        3. +-dplyr::mutate(rowwise(group_by(df, g = f():n())), x = f())
        4. +-dplyr::rowwise(group_by(df, g = f():n()))
        5. +-dplyr::group_by(df, g = f():n())
        6. +-dplyr:::group_by.data.frame(df, g = f():n())
        7. | \-dplyr::group_by_prepare(...)
        8. |   \-dplyr:::add_computed_columns(...)
        9. |     \-dplyr:::mutate_cols(...)
       10. |       +-base::withCallingHandlers(...)
       11. |       \-(function() {...
       12. |         \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
       13. |           \-mask$eval_all_mutate(quo)
       14. |             \-dplyr (local) eval()
       15. \-dplyr (local) f()
       16.   \-base::warning("msg")
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in row 1 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(...)
        2. +-dplyr::group_by(...)
        3. +-dplyr::mutate(rowwise(group_by(df, g = f():n())), x = f())
        4. +-dplyr:::mutate.data.frame(...)
        5. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        6. |   +-base::withCallingHandlers(...)
        7. |   \-(function() {...
        8. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        9. |       \-mask$eval_all_mutate(quo)
       10. |         \-dplyr (local) eval()
       11. \-dplyr (local) f()
       12.   \-base::warning("msg")
      
      [[3]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in row 2 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(...)
        2. +-dplyr::group_by(...)
        3. +-dplyr::mutate(rowwise(group_by(df, g = f():n())), x = f())
        4. +-dplyr:::mutate.data.frame(...)
        5. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        6. |   +-base::withCallingHandlers(...)
        7. |   \-(function() {...
        8. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        9. |       \-mask$eval_all_mutate(quo)
       10. |         \-dplyr (local) eval()
       11. \-dplyr (local) f()
       12.   \-base::warning("msg")
      
      [[4]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in group 1: id = 1 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(...)
        2. +-dplyr:::mutate.data.frame(...)
        3. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        4. |   +-base::withCallingHandlers(...)
        5. |   \-(function() {...
        6. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        7. |       \-mask$eval_all_mutate(quo)
        8. |         \-dplyr (local) eval()
        9. \-dplyr (local) f()
       10.   \-base::warning("msg")
      
      [[5]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in group 2: id = 2 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(...)
        2. +-dplyr:::mutate.data.frame(...)
        3. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        4. |   +-base::withCallingHandlers(...)
        5. |   \-(function() {...
        6. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        7. |       \-mask$eval_all_mutate(quo)
        8. |         \-dplyr (local) eval()
        9. \-dplyr (local) f()
       10.   \-base::warning("msg")
      

---

    Code
      # Truncated (1 more)
      mutate(rowwise(df), x = f())
    Condition
      Warning:
      There were 2 warnings in a `mutate()` step.
      The first warning was:
      ! msg
      i Run `dplyr::dplyr_last_warnings()` to see the 1 remaining warning.
    Output
      # A tibble: 2 x 2
      # Rowwise: 
           id     x
        <int> <dbl>
      1     1     1
      2     2     1
    Code
      dplyr_last_warnings(n = 1)
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in row 1 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(rowwise(df), x = f())
        2. +-dplyr:::mutate.data.frame(rowwise(df), x = f())
        3. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        4. |   +-base::withCallingHandlers(...)
        5. |   \-(function() {...
        6. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        7. |       \-mask$eval_all_mutate(quo)
        8. |         \-dplyr (local) eval()
        9. \-dplyr (local) f()
       10.   \-base::warning("msg")
      
    Message
      ... with 1 more warning.
      i Use `dplyr_last_warnings(n = ...)` to show more.

---

    Code
      # Truncated (several more)
      df <- tibble(id = 1:5)
      mutate(rowwise(df), x = f())
    Condition
      Warning:
      There were 5 warnings in a `mutate()` step.
      The first warning was:
      ! msg
      i Run `dplyr::dplyr_last_warnings()` to see the 4 remaining warnings.
    Output
      # A tibble: 5 x 2
      # Rowwise: 
           id     x
        <int> <dbl>
      1     1     1
      2     2     1
      3     3     1
      4     4     1
      5     5     1
    Code
      dplyr_last_warnings(n = 1)
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in row 1 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
           x
        1. +-dplyr::mutate(rowwise(df), x = f())
        2. +-dplyr:::mutate.data.frame(rowwise(df), x = f())
        3. | \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        4. |   +-base::withCallingHandlers(...)
        5. |   \-(function() {...
        6. |     \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        7. |       \-mask$eval_all_mutate(quo)
        8. |         \-dplyr (local) eval()
        9. \-dplyr (local) f()
       10.   \-base::warning("msg")
      
    Message
      ... with 4 more warnings.
      i Use `dplyr_last_warnings(n = ...)` to show more.

# complex backtraces with base and rlang warnings

    Code
      foo()
    Condition
      Warning:
      There was 1 warning in a `group_by()` step.
      ! foo
      Warning:
      There were 3 warnings in a `mutate()` step.
      The first warning was:
      ! foo
      i Run `dplyr::dplyr_last_warnings()` to see the 2 remaining warnings.
    Output
      # A tibble: 3 x 2
      # Groups:   x [1]
           id     x
        <int> <dbl>
      1     1     1
      2     2     1
      3     3     1
    Code
      dplyr_last_warnings()
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `group_by()`:
      Problem while computing `x = f(1):n()`.
      Caused by warning in `h()`:
      ! foo
      ---
      Backtrace:
           x
        1. +-dplyr (local) foo()
        2. | \-dplyr (local) bar()
        3. |   +-dplyr::mutate(group_by(df, x = f(1):n()), x = f(1, base = FALSE))
        4. |   +-dplyr::group_by(df, x = f(1):n())
        5. |   \-dplyr:::group_by.data.frame(df, x = f(1):n())
        6. |     \-dplyr::group_by_prepare(...)
        7. |       \-dplyr:::add_computed_columns(...)
        8. |         \-dplyr:::mutate_cols(...)
        9. |           +-base::withCallingHandlers(...)
       10. |           \-(function() {...
       11. |             \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
       12. |               \-mask$eval_all_mutate(quo)
       13. |                 \-dplyr (local) eval()
       14. \-dplyr (local) f(1)
       15.   \-dplyr (local) g(...)
       16.     \-dplyr (local) h(...)
       17.       \-base::warning("foo")
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in group 1: x = 1 while computing `x = f(1, base = FALSE)`.
      Caused by warning:
      ! foo
      ---
      Backtrace:
           x
        1. +-dplyr (local) foo()
        2. | \-dplyr (local) bar()
        3. |   +-dplyr::mutate(group_by(df, x = f(1):n()), x = f(1, base = FALSE))
        4. |   \-dplyr:::mutate.data.frame(...)
        5. |     \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        6. |       +-base::withCallingHandlers(...)
        7. |       \-(function() {...
        8. |         \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        9. |           \-mask$eval_all_mutate(quo)
       10. |             \-dplyr (local) eval()
       11. \-dplyr (local) f(1, base = FALSE)
       12.   \-dplyr (local) g(...)
       13.     \-dplyr (local) h(...)
       14.       \-rlang::warn("foo")
      
      [[3]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in group 2: x = 2 while computing `x = f(1, base = FALSE)`.
      Caused by warning:
      ! foo
      ---
      Backtrace:
           x
        1. +-dplyr (local) foo()
        2. | \-dplyr (local) bar()
        3. |   +-dplyr::mutate(group_by(df, x = f(1):n()), x = f(1, base = FALSE))
        4. |   \-dplyr:::mutate.data.frame(...)
        5. |     \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        6. |       +-base::withCallingHandlers(...)
        7. |       \-(function() {...
        8. |         \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        9. |           \-mask$eval_all_mutate(quo)
       10. |             \-dplyr (local) eval()
       11. \-dplyr (local) f(1, base = FALSE)
       12.   \-dplyr (local) g(...)
       13.     \-dplyr (local) h(...)
       14.       \-rlang::warn("foo")
      
      [[4]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in group 3: x = 3 while computing `x = f(1, base = FALSE)`.
      Caused by warning:
      ! foo
      ---
      Backtrace:
           x
        1. +-dplyr (local) foo()
        2. | \-dplyr (local) bar()
        3. |   +-dplyr::mutate(group_by(df, x = f(1):n()), x = f(1, base = FALSE))
        4. |   \-dplyr:::mutate.data.frame(...)
        5. |     \-dplyr:::mutate_cols(.data, dplyr_quosures(...), caller_env = caller_env())
        6. |       +-base::withCallingHandlers(...)
        7. |       \-(function() {...
        8. |         \-dplyr:::mutate_col(dots[[i]], .data, mask, new_columns)
        9. |           \-mask$eval_all_mutate(quo)
       10. |             \-dplyr (local) eval()
       11. \-dplyr (local) f(1, base = FALSE)
       12.   \-dplyr (local) g(...)
       13.     \-dplyr (local) h(...)
       14.       \-rlang::warn("foo")
      

