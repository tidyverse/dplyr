# pluralizes correctly

    Code
      enforce(df, x == 1, x > 3)
    Error <rlang_error>
      Enforcement failed. The following requirements were not met:
      x 1 row failed: x == 1.
      x 2 rows failed: x > 3.
      i Locate failures by calling `enforce_last()`.

# uses user provided messages if given

    Code
      enforce_show(df, x == 1, `x is greater than 3` = x > 1)
    Output
      # A tibble: 2 x 3
        requirement           row data$x
        <chr>               <int>  <int>
      1 x == 1                  2      2
      2 x is greater than 3     1      1

# can handle duplicate user messages

    Code
      enforce_show(df, oops = x == 1, oops = x > 1)
    Output
      # A tibble: 2 x 3
        requirement   row data$x
        <chr>       <int>  <int>
      1 oops...1        2      2
      2 oops...2        1      1

# can handle duplicate expressions

    Code
      enforce_show(df, x == 1, x == 1)
    Output
      # A tibble: 2 x 3
        requirement   row data$x
        <chr>       <int>  <int>
      1 x == 1...1      2      2
      2 x == 1...2      2      2

# fails if no failures have been recorded

    Code
      enforce_last()
    Error <rlang_error>
      Can't show enforcement failure information because no failures have been recorded yet.

