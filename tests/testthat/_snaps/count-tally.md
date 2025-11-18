# name must be string

    Code
      count(df, x, name = 1)
    Condition
      Error in `tally()`:
      ! `name` must be a single string, not the number 1.

---

    Code
      count(df, x, name = letters)
    Condition
      Error in `tally()`:
      ! `name` must be a single string, not a character vector.

# can only explicitly chain together multiple tallies

    Code
      df <- data.frame(g = c(1, 1, 2, 2), n = 1:4)
      count(df, g, wt = n)
    Output
        g n
      1 1 3
      2 2 7
    Code
      count(count(df, g, wt = n), wt = n)
    Output
         n
      1 10
    Code
      count(df, n)
    Message
      Storing counts in `nn`, as `n` already present in input
      i Use `name = "new_name"` to pick a new name.
    Output
        n nn
      1 1  1
      2 2  1
      3 3  1
      4 4  1

# count() owns errors (#6139)

    Code
      (expect_error(count(mtcars, new = 1 + "")))
    Output
      <error/dplyr:::mutate_error>
      Error in `count()`:
      i In argument: `new = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      (expect_error(count(mtcars, wt = 1 + "")))
    Output
      <error/rlang_error>
      Error in `count()`:
      i In argument: `n = base::sum(1 + "", na.rm = TRUE)`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

# count() `wt = n()` is deprecated

    Code
      count(df, a, wt = n())
    Condition
      Warning:
      `wt = n()` was deprecated in dplyr 1.0.1.
      i You can now omit the `wt` argument.
    Output
      # A tibble: 5 x 2
            a     n
        <int> <int>
      1     1     1
      2     2     1
      3     3     1
      4     4     1
      5     5     1

# tally() owns errors (#6139)

    Code
      (expect_error(tally(mtcars, wt = 1 + "")))
    Output
      <error/rlang_error>
      Error in `tally()`:
      i In argument: `n = base::sum(1 + "", na.rm = TRUE)`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

# tally() `wt = n()` is deprecated

    Code
      tally(df, wt = n())
    Condition
      Warning:
      `wt = n()` was deprecated in dplyr 1.0.1.
      i You can now omit the `wt` argument.
    Output
      # A tibble: 1 x 1
            n
        <int>
      1     5

# `.drop` is defunct

    Code
      add_count(df, f, .drop = FALSE)
    Condition
      Error:
      ! The `.drop` argument of `add_count()` was deprecated in dplyr 1.0.0 and is now defunct.

# add_count() `wt = n()` is deprecated

    Code
      add_count(df, a, wt = n())
    Condition
      Warning:
      `wt = n()` was deprecated in dplyr 1.0.1.
      i You can now omit the `wt` argument.
    Output
      # A tibble: 5 x 2
            a     n
        <int> <int>
      1     1     1
      2     2     1
      3     3     1
      4     4     1
      5     5     1

# add_count() owns errors (#6139)

    Code
      (expect_error(add_count(mtcars, new = 1 + "")))
    Output
      <error/dplyr:::mutate_error>
      Error in `add_count()`:
      i In argument: `new = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      (expect_error(add_count(mtcars, wt = 1 + "")))
    Output
      <error/dplyr:::mutate_error>
      Error in `add_count()`:
      i In argument: `n = base::sum(1 + "", na.rm = TRUE)`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

# add_tally() owns errors (#6139)

    Code
      (expect_error(add_tally(mtcars, wt = 1 + "")))
    Output
      <error/dplyr:::mutate_error>
      Error in `add_tally()`:
      i In argument: `n = base::sum(1 + "", na.rm = TRUE)`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

# add_tally() `wt = n()` is deprecated

    Code
      add_tally(df, wt = n())
    Condition
      Warning:
      `wt = n()` was deprecated in dplyr 1.0.1.
      i You can now omit the `wt` argument.
    Output
      # A tibble: 5 x 2
            a     n
        <int> <int>
      1     1     5
      2     2     5
      3     3     5
      4     4     5
      5     5     5

