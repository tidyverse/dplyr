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
      Warning in `mutate()`:
      Problem while computing `x = f()`.
      Caused by warning in `f()`:
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

    Code
      # Grouped
      invisible(mutate(group_by(df, id), x = f()))
    Condition
      Warning:
      There were 2 warnings in `mutate()`.
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
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in group 2: id = 2 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      

---

    Code
      # Rowwise
      invisible(mutate(rowwise(df), x = f()))
    Condition
      Warning:
      There were 2 warnings in `mutate()`.
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
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in row 2 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      

---

    Code
      # Multiple type of warnings within multiple verbs
      invisible(mutate(group_by(mutate(rowwise(group_by(df, g = f():n())), x = f()),
      id), x = f()))
    Condition
      Warning in `group_by()`:
      Problem while computing `g = f():n()`.
      Caused by warning in `f()`:
      ! msg
      Warning:
      There were 2 warnings in `mutate()`.
      The first warning was:
      ! msg
      i Run `dplyr::dplyr_last_warnings()` to see the 1 remaining warning.
      Warning:
      There were 2 warnings in `mutate()`.
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
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in row 1 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      
      [[3]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in row 2 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      
      [[4]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in group 1: id = 1 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      
      [[5]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      Problem in group 2: id = 2 while computing `x = f()`.
      Caused by warning in `f()`:
      ! msg
      

---

    Code
      # Truncated (1 more)
      mutate(rowwise(df), x = f())
    Condition
      Warning:
      There were 2 warnings in `mutate()`.
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
      There were 5 warnings in `mutate()`.
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
      
    Message
      ... with 4 more warnings.
      i Use `dplyr_last_warnings(n = ...)` to show more.

