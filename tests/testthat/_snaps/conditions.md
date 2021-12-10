# can hide expression in error messages

    Code
      mutate(mtcars, invisible(999 + ""))
    Condition
      Error in `mutate()`:
        Problem while computing `..1`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      summarise(mtcars, invisible(999 + ""))
    Condition
      Error in `summarise()`:
        Problem while computing `..1`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      filter(mtcars, invisible(999 + ""))
    Condition
      Error in `filter()`:
        Problem while computing `..1`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      arrange(mtcars, invisible(999 + ""))
    Condition
      Error in `arrange()`:
        Problem with the implicit `transmute()` step.
        x Problem while computing `..1`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      select(mtcars, invisible(999 + ""))
    Condition
      Error in `select()`: non-numeric argument to binary operator
    Code
      slice(mtcars, invisible(999 + ""))
    Condition
      Error in `slice()`:
        Problem while evaluating `..1`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      mutate(mtcars, var = invisible(999 + ""))
    Condition
      Error in `mutate()`:
        Problem while computing `var`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      summarise(mtcars, var = invisible(999 + ""))
    Condition
      Error in `summarise()`:
        Problem while computing `var`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      filter(mtcars, var = invisible(999 + ""))
    Condition
      Error in `filter()`: We detected a named input.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `var == invisible(999 + "")`?
    Code
      arrange(mtcars, var = invisible(999 + ""))
    Condition
      Error in `arrange()`:
        Problem with the implicit `transmute()` step.
        x Problem while computing `..1`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      select(mtcars, var = invisible(999 + ""))
    Condition
      Error in `select()`: non-numeric argument to binary operator
    Code
      slice(mtcars, var = invisible(999 + ""))
    Condition
      Error in `slice()`:
        Problem while evaluating `var`.
      Caused by error in `+`:
        non-numeric argument to binary operator

