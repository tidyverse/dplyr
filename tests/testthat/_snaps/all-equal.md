# data frames not equal if missing row

    Code
      all_equal(mtcars, mtcars[-1, ])
    Output
      [1] "Different number of rows."
    Code
      all_equal(iris, iris[-1, ])
    Output
      [1] "Different number of rows."
    Code
      all_equal(df_all, df_all[-1, ])
    Output
      [1] "Different number of rows."

# data frames not equal if missing col

    Code
      all_equal(mtcars, mtcars[, -1])
    Output
      Different number of columns: 11 vs 10.
    Code
      all_equal(iris, iris[, -1])
    Output
      Different number of columns: 5 vs 4.
    Code
      all_equal(df_all, df_all[, -1])
    Output
      Different number of columns: 7 vs 6.

# factors equal only if levels equal

    Code
      all_equal(df1, df2)
    Output
      Different types for column `x`: factor<38051> vs factor<bb232>.
    Code
      all_equal(df2, df1)
    Output
      Different types for column `x`: factor<bb232> vs factor<38051>.

# factor comparison requires strict equality of levels (#2440)

    Code
      all_equal(df1, df2)
    Output
      Different types for column `x`: factor<4d52a> vs factor<38051>.
    Code
      all_equal(df2, df1)
    Output
      Different types for column `x`: factor<38051> vs factor<4d52a>.

# equality test fails when convert is FALSE and types don't match (#1484)

    Code
      all_equal(df1, df2, convert = FALSE)
    Output
      Different types for column `x`: character vs factor<4d52a>.

# equality returns a message for convert = TRUE

    Code
      all_equal(df1, df2)
    Output
      Different types for column `x`: integer vs character.
    Code
      all_equal(df1, df2, convert = TRUE)
    Output
      Incompatible types for column `x`: integer vs character.

# numeric and integer can be compared if convert = TRUE

    Code
      all_equal(df1, df2)
    Output
      Different types for column `x`: integer vs double.

# returns vector for more than one difference (#1819)

    Code
      all_equal(tibble(a = 1, b = 2), tibble(a = 1L, b = 2L))
    Output
      Different types for column `a`: double vs integer.
      Different types for column `b`: double vs integer.

# ignore column order

    Code
      all_equal(tibble(a = 1, b = 2), tibble(b = 2, a = 1), ignore_col_order = FALSE)
    Output
      Same column names, but different order.
    Code
      all_equal(tibble(a = 1, b = 2), tibble(a = 1), ignore_col_order = FALSE)
    Output
      Different number of columns: 2 vs 1.

# count() give meaningful errors

    Code
      (expect_error(union(tibble(a = 1), tibble(a = "1"))))
    Output
      <error/rlang_error>
      Error in `union()`:
      ! `x` and `y` are not compatible.
      x Incompatible types for column `a`: double vs character.
    Code
      (expect_error(union(tibble(a = 1, b = 2), tibble(a = "1", b = "2"))))
    Output
      <error/rlang_error>
      Error in `union()`:
      ! `x` and `y` are not compatible.
      x Incompatible types for column `a`: double vs character.
      x Incompatible types for column `b`: double vs character.

