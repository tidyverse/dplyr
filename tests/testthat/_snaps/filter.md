# filter() allows matrices with 1 column with a deprecation warning (#6091)

    Code
      out <- filter(df, matrix(c(TRUE, FALSE), nrow = 2))
    Condition
      Warning:
      Using one column matrices in `filter()` was deprecated in dplyr 1.1.0.
      i Please use one dimensional logical vectors instead.

---

    Code
      out <- filter(gdf, matrix(c(TRUE, FALSE), nrow = 2))
    Condition
      Warning:
      Using one column matrices in `filter()` was deprecated in dplyr 1.1.0.
      i Please use one dimensional logical vectors instead.

# filter() disallows matrices with >1 column

    Code
      (expect_error(filter(df, matrix(TRUE, nrow = 3, ncol = 2))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `matrix(TRUE, nrow = 3, ncol = 2)`.
      Caused by error:
      ! `..1` must be a logical vector, not a logical matrix.

# filter() disallows arrays with >2 dimensions

    Code
      (expect_error(filter(df, array(TRUE, dim = c(3, 1, 1)))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `array(TRUE, dim = c(3, 1, 1))`.
      Caused by error:
      ! `..1` must be a logical vector, not a logical array.

# filter() gives useful error messages

    Code
      (expect_error(iris %>% group_by(Species) %>% filter(1:n())))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `1:n()`.
      i In group 1: `Species = setosa`.
      Caused by error:
      ! `..1` must be a logical vector, not an integer vector.
    Code
      (expect_error(iris %>% filter(1:n())))
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
      (expect_error(iris %>% group_by(Species) %>% filter(c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `c(TRUE, FALSE)`.
      i In group 1: `Species = setosa`.
      Caused by error:
      ! `..1` must be of size 50 or 1, not size 2.
    Code
      (expect_error(iris %>% rowwise(Species) %>% filter(c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `c(TRUE, FALSE)`.
      i In row 1.
      Caused by error:
      ! `..1` must be of size 1, not size 2.
    Code
      (expect_error(iris %>% filter(c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `c(TRUE, FALSE)`.
      Caused by error:
      ! `..1` must be of size 150 or 1, not size 2.
    Code
      (expect_error(iris %>% group_by(Species) %>% filter(data.frame(c(TRUE, FALSE))))
      )
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `data.frame(c(TRUE, FALSE))`.
      i In group 1: `Species = setosa`.
      Caused by error:
      ! `..1` must be of size 50 or 1, not size 2.
    Code
      (expect_error(iris %>% rowwise() %>% filter(data.frame(c(TRUE, FALSE)))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `data.frame(c(TRUE, FALSE))`.
      i In row 1.
      Caused by error:
      ! `..1` must be of size 1, not size 2.
    Code
      (expect_error(iris %>% filter(data.frame(c(TRUE, FALSE)))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `data.frame(c(TRUE, FALSE))`.
      Caused by error:
      ! `..1` must be of size 150 or 1, not size 2.
    Code
      (expect_error(tibble(x = 1) %>% filter(c(TRUE, TRUE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `c(TRUE, TRUE)`.
      Caused by error:
      ! `..1` must be of size 1, not size 2.
    Code
      (expect_error(iris %>% group_by(Species) %>% filter(data.frame(Sepal.Length > 3,
      1:n()))))
    Condition
      Warning:
      Returning data frames from `filter()` expressions was deprecated in dplyr 1.0.8.
      i Please use `if_any()` or `if_all()` instead.
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `data.frame(Sepal.Length > 3, 1:n())`.
      i In group 1: `Species = setosa`.
      Caused by error:
      ! `..1$X1.n..` must be a logical vector, not an integer vector.
    Code
      (expect_error(iris %>% filter(data.frame(Sepal.Length > 3, 1:n()))))
    Condition
      Warning:
      Returning data frames from `filter()` expressions was deprecated in dplyr 1.0.8.
      i Please use `if_any()` or `if_all()` instead.
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `data.frame(Sepal.Length > 3, 1:n())`.
      Caused by error:
      ! `..1$X1.n..` must be a logical vector, not an integer vector.
    Code
      (expect_error(mtcars %>% filter(`_x`)))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `_x`.
      Caused by error:
      ! object '_x' not found
    Code
      (expect_error(mtcars %>% group_by(cyl) %>% filter(`_x`)))
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
      (expect_error(tibble() %>% filter(stop("{"))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      i In argument: `stop("{")`.
      Caused by error:
      ! {
    Code
      data.frame(x = 1, y = 1) %>% filter(across(everything(), ~ .x > 0))
    Condition
      Warning:
      Using `across()` in `filter()` was deprecated in dplyr 1.0.8.
      i Please use `if_any()` or `if_all()` instead.
    Output
        x y
      1 1 1
    Code
      data.frame(x = 1, y = 1) %>% filter(data.frame(x > 0, y > 0))
    Condition
      Warning:
      Returning data frames from `filter()` expressions was deprecated in dplyr 1.0.8.
      i Please use `if_any()` or `if_all()` instead.
    Output
        x y
      1 1 1

# can't use `.by` with `.preserve`

    Code
      filter(df, .by = x, .preserve = TRUE)
    Condition
      Error in `filter()`:
      ! Can't supply both `.by` and `.preserve`.

# catches `.by` with grouped-df

    Code
      filter(gdf, .by = x)
    Condition
      Error in `filter()`:
      ! Can't supply `.by` when `.data` is a grouped data frame.

# catches `.by` with rowwise-df

    Code
      filter(rdf, .by = x)
    Condition
      Error in `filter()`:
      ! Can't supply `.by` when `.data` is a rowwise data frame.

# catches `by` typo (#6647)

    Code
      filter(df, by = x)
    Condition
      Error in `filter()`:
      ! Can't specify an argument named `by` in this verb.
      i Did you mean to use `.by` instead?

