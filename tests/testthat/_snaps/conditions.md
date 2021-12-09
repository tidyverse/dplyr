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

# can pass verb-level error call

    Code
      mutate(mtcars, 1 + "", .error_call = call("foo"))
    Condition
      Error in `foo()`:
        Problem while computing `..1 = 1 + ""`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      transmute(mtcars, 1 + "", .error_call = call("foo"))
    Condition
      Error in `foo()`:
        Problem while computing `..1 = 1 + ""`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      summarise(mtcars, 1 + "", .error_call = call("foo"))
    Condition
      Error in `foo()`:
        Problem while computing `..1 = 1 + ""`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      filter(mtcars, 1 + "", .error_call = call("foo"))
    Condition
      Error in `foo()`:
        Problem while computing `..1 = 1 + ""`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      arrange(mtcars, 1 + "", .error_call = call("foo"))
    Condition
      Error in `foo()`:
        Problem with the implicit `transmute()` step.
        x Problem while computing `..1 = 1 + ""`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      select(mtcars, 1 + "", .error_call = call("foo"))
    Condition
      Error in `foo()`: non-numeric argument to binary operator
    Code
      slice(mtcars, 1 + "", .error_call = call("foo"))
    Condition
      Error in `foo()`:
        Problem evaluating `... = <fn>` .
      Caused by error in `+`:
        non-numeric argument to binary operator

# can pass verb-level error call (example case)

    Code
      my_verb(mtcars, 1 + "", am)
    Condition
      Error in `my_verb()`:
        Problem while computing `.result = (1 + "") * am`.
      Caused by error in `+`:
        non-numeric argument to binary operator
    Code
      my_verb(mtcars, cyl, c(am, vs))
    Condition
      Error in `my_verb()`: Problem while computing `.result = cyl * c(am, vs)`.
      i `.result` must be size 32 or 1, not 64.

