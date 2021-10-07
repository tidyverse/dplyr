# filter() gives useful error messages

    Code
      (expect_error(iris %>% group_by(Species) %>% filter(1:n())))
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `1:n()`.
      x Input `..1` must be a logical vector, not a integer.
      i The error occurred in group 1: Species = setosa.
    Code
      (expect_error(iris %>% filter(1:n())))
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `1:n()`.
      x Input `..1` must be a logical vector, not a integer.
    Code
      (expect_error(iris %>% group_by(Species) %>% filter(c(TRUE, FALSE))))
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `c(TRUE, FALSE)`.
      x Input `..1` must be of size 50 or 1, not size 2.
      i The error occurred in group 1: Species = setosa.
    Code
      (expect_error(iris %>% rowwise(Species) %>% filter(c(TRUE, FALSE))))
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `c(TRUE, FALSE)`.
      x Input `..1` must be of size 1, not size 2.
      i The error occurred in row 1.
    Code
      (expect_error(iris %>% filter(c(TRUE, FALSE))))
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `c(TRUE, FALSE)`.
      x Input `..1` must be of size 150 or 1, not size 2.
    Code
      (expect_error(iris %>% group_by(Species) %>% filter(data.frame(c(TRUE, FALSE))))
      )
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `data.frame(c(TRUE, FALSE))`.
      x Input `..1` must be of size 50 or 1, not size 2.
      i The error occurred in group 1: Species = setosa.
    Code
      (expect_error(iris %>% rowwise() %>% filter(data.frame(c(TRUE, FALSE)))))
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `data.frame(c(TRUE, FALSE))`.
      x Input `..1` must be of size 1, not size 2.
      i The error occurred in row 1.
    Code
      (expect_error(iris %>% filter(data.frame(c(TRUE, FALSE)))))
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `data.frame(c(TRUE, FALSE))`.
      x Input `..1` must be of size 150 or 1, not size 2.
    Code
      (expect_error(tibble(x = 1) %>% filter(c(TRUE, TRUE))))
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `c(TRUE, TRUE)`.
      x Input `..1` must be of size 1, not size 2.
    Code
      (expect_error(iris %>% group_by(Species) %>% filter(data.frame(Sepal.Length > 3,
      1:n()))))
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `data.frame(Sepal.Length > 3, 1:n())`.
      x Input `..1$X1.n..` must be a logical vector, not a integer.
      i The error occurred in group 1: Species = setosa.
    Code
      (expect_error(iris %>% filter(data.frame(Sepal.Length > 3, 1:n()))))
    Output
      <error/dplyr_error>
      Error: Problem with `filter()` input `..1`.
      i Input `..1` is `data.frame(Sepal.Length > 3, 1:n())`.
      x Input `..1$X1.n..` must be a logical vector, not a integer.
    Code
      (expect_error(mtcars %>% filter(`_x`)))
    Output
      <error/dplyr_error>
      Error in `h()`: Problem with `filter()` input `..1`.
      i Input `..1` is `_x`.
      x object '_x' not found
    Code
      (expect_error(mtcars %>% group_by(cyl) %>% filter(`_x`)))
    Output
      <error/dplyr_error>
      Error in `h()`: Problem with `filter()` input `..1`.
      i Input `..1` is `_x`.
      x object '_x' not found
      i The error occurred in group 1: cyl = 4.
    Code
      (expect_error(filter(mtcars, x = 1)))
    Output
      <error/rlang_error>
      Error in `check_filter()`: Problem with `filter()` input `..1`.
      x Input `..1` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?
    Code
      (expect_error(filter(mtcars, y > 2, z = 3)))
    Output
      <error/rlang_error>
      Error in `check_filter()`: Problem with `filter()` input `..2`.
      x Input `..2` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `z == 3`?
    Code
      (expect_error(filter(mtcars, TRUE, x = 1)))
    Output
      <error/rlang_error>
      Error in `check_filter()`: Problem with `filter()` input `..2`.
      x Input `..2` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?
    Code
      (expect_error(filter(ts(1:10))))
    Output
      <error/rlang_error>
      Error in `filter.ts()`: Problem with `filter()` input `.data`.
      x `.data` is a <ts> object, not a data source.
      i Did you want to use `stats::filter()`?
    Code
      (expect_error(tibble() %>% filter(stop("{"))))
    Output
      <error/dplyr_error>
      Error in `h()`: Problem with `filter()` input `..1`.
      i Input `..1` is `stop("{")`.
      x {
    Code
      data.frame(x = 1, y = 1) %>% filter(across(everything(), ~ .x > 0))
    Warning <simpleWarning>
      Using `across()` in `filter()` is deprecated, use `if_any()` or `if_all()`.
    Output
        x y
      1 1 1
    Code
      data.frame(x = 1, y = 1) %>% filter(data.frame(x > 0, y > 0))
    Warning <simpleWarning>
      data frame results in `filter()` are deprecated, use `if_any()` or `if_all()`.
    Output
        x y
      1 1 1

