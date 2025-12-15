# filter() and filter_out() allow matrices with 1 column with a deprecation warning (#6091)

    Code
      out <- filter(df, matrix(c(TRUE, FALSE), nrow = 2))
    Condition
      Warning:
      Using one column matrices in `filter()` or `filter_out()` was deprecated in dplyr 1.1.0.
      i Please use one dimensional logical vectors instead.

---

    Code
      out <- filter_out(df, matrix(c(TRUE, FALSE), nrow = 2))
    Condition
      Warning:
      Using one column matrices in `filter()` or `filter_out()` was deprecated in dplyr 1.1.0.
      i Please use one dimensional logical vectors instead.

---

    Code
      out <- filter(gdf, matrix(c(TRUE, FALSE), nrow = 2))
    Condition
      Warning:
      Using one column matrices in `filter()` or `filter_out()` was deprecated in dplyr 1.1.0.
      i Please use one dimensional logical vectors instead.

---

    Code
      out <- filter_out(gdf, matrix(c(TRUE, FALSE), nrow = 2))
    Condition
      Warning:
      Using one column matrices in `filter()` or `filter_out()` was deprecated in dplyr 1.1.0.
      i Please use one dimensional logical vectors instead.

# filter() and filter_out() disallow matrices with >1 column

    Code
      filter(df, matrix(TRUE, nrow = 3, ncol = 2))
    Condition
      Error in `filter()`:
      i In argument: `matrix(TRUE, nrow = 3, ncol = 2)`.
      Caused by error:
      ! `..1` must be a logical vector, not a logical matrix.

---

    Code
      filter_out(df, matrix(TRUE, nrow = 3, ncol = 2))
    Condition
      Error in `filter_out()`:
      i In argument: `matrix(TRUE, nrow = 3, ncol = 2)`.
      Caused by error:
      ! `..1` must be a logical vector, not a logical matrix.

# filter() and filter_out() disallow arrays with >2 dimensions

    Code
      filter(df, array(TRUE, dim = c(3, 1, 1)))
    Condition
      Error in `filter()`:
      i In argument: `array(TRUE, dim = c(3, 1, 1))`.
      Caused by error:
      ! `..1` must be a logical vector, not a logical array.

---

    Code
      filter_out(df, array(TRUE, dim = c(3, 1, 1)))
    Condition
      Error in `filter_out()`:
      i In argument: `array(TRUE, dim = c(3, 1, 1))`.
      Caused by error:
      ! `..1` must be a logical vector, not a logical array.

# filter() gives useful error messages

    Code
      (expect_error(filter(group_by(iris, Species), 1:n())))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `1:n()`.
      i In group 1: `Species = setosa`.
      Caused by error:
      ! `..1` must be a logical vector, not an integer vector.
    Code
      (expect_error(filter(iris, 1:n())))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `1:n()`.
      Caused by error:
      ! `..1` must be a logical vector, not an integer vector.
    Code
      (expect_error(filter(data.frame(x = 1:2), matrix(c(TRUE, FALSE, TRUE, FALSE),
      nrow = 2))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2)`.
      Caused by error:
      ! `..1` must be a logical vector, not a logical matrix.
    Code
      (expect_error(filter(group_by(iris, Species), c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `c(TRUE, FALSE)`.
      i In group 1: `Species = setosa`.
      Caused by error:
      ! `..1` must be of size 50 or 1, not size 2.
    Code
      (expect_error(filter(rowwise(iris, Species), c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `c(TRUE, FALSE)`.
      i In row 1.
      Caused by error:
      ! `..1` must be of size 1, not size 2.
    Code
      (expect_error(filter(iris, c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `c(TRUE, FALSE)`.
      Caused by error:
      ! `..1` must be of size 150 or 1, not size 2.
    Code
      (expect_error(filter(mtcars, `_x`)))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `_x`.
      Caused by error:
      ! object '_x' not found
    Code
      (expect_error(filter(group_by(mtcars, cyl), `_x`)))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `_x`.
      i In group 1: `cyl = 4`.
      Caused by error:
      ! object '_x' not found
    Code
      (expect_error(filter(mtcars, x = 1)))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! We detected a named input.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?
    Code
      (expect_error(filter(mtcars, y > 2, z = 3)))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! We detected a named input.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `z == 3`?
    Code
      (expect_error(filter(mtcars, TRUE, x = 1)))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! We detected a named input.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?
    Code
      (expect_error(filter(ts(1:10))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Incompatible data source.
      x `.data` is a <ts> object, not a data source.
      i Did you want to use `stats::filter()`?
    Code
      (expect_error(filter(tibble(), stop("{"))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `stop("{")`.
      Caused by error:
      ! {

# Using data frames in `filter()` is defunct (#7758)

    Code
      filter(df, across(everything(), ~ .x > 0))
    Condition
      Error in `filter()`:
      i In argument: `across(everything(), ~.x > 0)`.
      Caused by error:
      ! `..1` must be a logical vector, not a <tbl_df/tbl/data.frame> object.
      i If you used `across()` to generate this data frame, please use `if_any()` or `if_all()` instead.

---

    Code
      filter(gdf, across(everything(), ~ .x > 0))
    Condition
      Error in `filter()`:
      i In argument: `across(everything(), ~.x > 0)`.
      i In group 1: `x = 1`.
      Caused by error:
      ! `..1` must be a logical vector, not a <tbl_df/tbl/data.frame> object.
      i If you used `across()` to generate this data frame, please use `if_any()` or `if_all()` instead.

---

    Code
      filter(rdf, across(everything(), ~ .x > 0))
    Condition
      Error in `filter()`:
      i In argument: `across(everything(), ~.x > 0)`.
      i In row 1.
      Caused by error:
      ! `..1` must be a logical vector, not a <tbl_df/tbl/data.frame> object.
      i If you used `across()` to generate this data frame, please use `if_any()` or `if_all()` instead.

---

    Code
      filter(df, tibble(x > 0, y > 0))
    Condition
      Error in `filter()`:
      i In argument: `tibble(x > 0, y > 0)`.
      Caused by error:
      ! `..1` must be a logical vector, not a <tbl_df/tbl/data.frame> object.
      i If you used `across()` to generate this data frame, please use `if_any()` or `if_all()` instead.

---

    Code
      filter(gdf, tibble(x > 0, y > 0))
    Condition
      Error in `filter()`:
      i In argument: `tibble(x > 0, y > 0)`.
      i In group 1: `x = 1`.
      Caused by error:
      ! `..1` must be a logical vector, not a <tbl_df/tbl/data.frame> object.
      i If you used `across()` to generate this data frame, please use `if_any()` or `if_all()` instead.

---

    Code
      filter(rdf, tibble(x > 0, y > 0))
    Condition
      Error in `filter()`:
      i In argument: `tibble(x > 0, y > 0)`.
      i In row 1.
      Caused by error:
      ! `..1` must be a logical vector, not a <tbl_df/tbl/data.frame> object.
      i If you used `across()` to generate this data frame, please use `if_any()` or `if_all()` instead.

# `filter()` doesn't allow data frames with missing or empty names (#6758)

    Code
      filter(df1)
    Condition
      Error in `filter()`:
      ! Can't transform a data frame with `NA` or `""` names.

---

    Code
      filter_out(df1)
    Condition
      Error in `filter_out()`:
      ! Can't transform a data frame with `NA` or `""` names.

---

    Code
      filter(df2)
    Condition
      Error in `filter()`:
      ! Can't transform a data frame with missing names.

---

    Code
      filter_out(df2)
    Condition
      Error in `filter_out()`:
      ! Can't transform a data frame with missing names.

# can't use `.by` with `.preserve`

    Code
      filter(df, .by = x, .preserve = TRUE)
    Condition
      Error in `filter()`:
      ! Can't supply both `.by` and `.preserve`.

---

    Code
      filter_out(df, .by = x, .preserve = TRUE)
    Condition
      Error in `filter_out()`:
      ! Can't supply both `.by` and `.preserve`.

# catches `.by` with grouped-df

    Code
      filter(gdf, .by = x)
    Condition
      Error in `filter()`:
      ! Can't supply `.by` when `.data` is a grouped data frame.

---

    Code
      filter_out(gdf, .by = x)
    Condition
      Error in `filter_out()`:
      ! Can't supply `.by` when `.data` is a grouped data frame.

# catches `.by` with rowwise-df

    Code
      filter(rdf, .by = x)
    Condition
      Error in `filter()`:
      ! Can't supply `.by` when `.data` is a rowwise data frame.

---

    Code
      filter_out(rdf, .by = x)
    Condition
      Error in `filter_out()`:
      ! Can't supply `.by` when `.data` is a rowwise data frame.

# catches `by` typo (#6647)

    Code
      filter(df, by = x)
    Condition
      Error in `filter()`:
      ! Can't specify an argument named `by` in this verb.
      i Did you mean to use `.by` instead?

---

    Code
      filter_out(df, by = x)
    Condition
      Error in `filter_out()`:
      ! Can't specify an argument named `by` in this verb.
      i Did you mean to use `.by` instead?

