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
      ! Problem with the implicit `transmute()` step.
      x Problem while computing `..1`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      select(mtcars, invisible(999 + ""))
    Condition
      Error in `select()`:
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
      ! Problem with the implicit `transmute()` step.
      x Problem while computing `..1`.
      Caused by error in `999 + ""`:
      ! non-numeric argument to binary operator
    Code
      select(mtcars, var = invisible(999 + ""))
    Condition
      Error in `select()`:
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
      ! Problem with the implicit `transmute()` step.
      x Problem while computing `..1 = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      select(mtcars, 1 + "")
    Condition
      Error in `foo()`:
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

