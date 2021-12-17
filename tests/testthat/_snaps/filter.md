# filter() gives useful error messages

    Code
      (expect_error(iris %>% group_by(Species) %>% filter(1:n())))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = 1:n()`.
      x Input `..1` must be a logical vector, not a integer.
      i The error occurred in group 1: Species = setosa.
    Code
      (expect_error(iris %>% filter(1:n())))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = 1:n()`.
      x Input `..1` must be a logical vector, not a integer.
    Code
      (expect_error(filter(data.frame(x = 1:2), matrix(c(TRUE, FALSE, TRUE, FALSE),
      nrow = 2))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2)`.
      x Input `..1` must be a logical vector, not a logical[,2].
    Code
      (expect_error(iris %>% group_by(Species) %>% filter(c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = c(TRUE, FALSE)`.
      x Input `..1` must be of size 50 or 1, not size 2.
      i The error occurred in group 1: Species = setosa.
    Code
      (expect_error(iris %>% rowwise(Species) %>% filter(c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = c(TRUE, FALSE)`.
      x Input `..1` must be of size 1, not size 2.
      i The error occurred in row 1.
    Code
      (expect_error(iris %>% filter(c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = c(TRUE, FALSE)`.
      x Input `..1` must be of size 150 or 1, not size 2.
    Code
      (expect_error(iris %>% group_by(Species) %>% filter(data.frame(c(TRUE, FALSE))))
      )
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = data.frame(c(TRUE, FALSE))`.
      x Input `..1` must be of size 50 or 1, not size 2.
      i The error occurred in group 1: Species = setosa.
    Code
      (expect_error(iris %>% rowwise() %>% filter(data.frame(c(TRUE, FALSE)))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = data.frame(c(TRUE, FALSE))`.
      x Input `..1` must be of size 1, not size 2.
      i The error occurred in row 1.
    Code
      (expect_error(iris %>% filter(data.frame(c(TRUE, FALSE)))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = data.frame(c(TRUE, FALSE))`.
      x Input `..1` must be of size 150 or 1, not size 2.
    Code
      (expect_error(tibble(x = 1) %>% filter(c(TRUE, TRUE))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = c(TRUE, TRUE)`.
      x Input `..1` must be of size 1, not size 2.
    Code
      (expect_error(iris %>% group_by(Species) %>% filter(data.frame(Sepal.Length > 3,
      1:n()))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = data.frame(Sepal.Length > 3, 1:n())`.
      x Input `..1$X1.n..` must be a logical vector, not a integer.
      i The error occurred in group 1: Species = setosa.
    Code
      (expect_error(iris %>% filter(data.frame(Sepal.Length > 3, 1:n()))))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = data.frame(Sepal.Length > 3, 1:n())`.
      x Input `..1$X1.n..` must be a logical vector, not a integer.
    Code
      (expect_error(mtcars %>% filter(`_x`)))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = _x`.
      Caused by error:
      ! object '_x' not found
    Code
      (expect_error(mtcars %>% group_by(cyl) %>% filter(`_x`)))
    Output
      <error/rlang_error>
      Error in `filter()`:
      ! Problem while computing `..1 = _x`.
      i The error occurred in group 1: cyl = 4.
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
      ! Problem while computing `..1 = stop("{")`.
      Caused by error:
      ! {
    Code
      data.frame(x = 1, y = 1) %>% filter(across(everything(), ~ .x > 0))
    Condition
      Warning:
      Using `across()` in `filter()` is deprecated, use `if_any()` or `if_all()`.
    Output
        x y
      1 1 1
    Code
      data.frame(x = 1, y = 1) %>% filter(data.frame(x > 0, y > 0))
    Condition
      Warning:
      data frame results in `filter()` are deprecated, use `if_any()` or `if_all()`.
    Output
        x y
      1 1 1

