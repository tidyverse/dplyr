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
      df %>% count(g, wt = n)
    Output
        g n
      1 1 3
      2 2 7
    Code
      df %>% count(g, wt = n) %>% count(wt = n)
    Output
         n
      1 10
    Code
      df %>% count(n)
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
      i In argument: `n = sum(1 + "", na.rm = TRUE)`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

# tally() owns errors (#6139)

    Code
      (expect_error(tally(mtcars, wt = 1 + "")))
    Output
      <error/rlang_error>
      Error in `tally()`:
      i In argument: `n = sum(1 + "", na.rm = TRUE)`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

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
      i In argument: `n = sum(1 + "", na.rm = TRUE)`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

# add_tally() owns errors (#6139)

    Code
      (expect_error(add_tally(mtcars, wt = 1 + "")))
    Output
      <error/dplyr:::mutate_error>
      Error in `add_tally()`:
      i In argument: `n = sum(1 + "", na.rm = TRUE)`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

