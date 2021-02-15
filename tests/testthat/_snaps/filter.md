# filter() gives useful error messages

    Code
      iris %>% group_by(Species) %>% filter(1:n())
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1` must be a logical vector, not a integer.
      i Input `..1` is `1:n()`.
      i The error occurred in group 1: Species = "setosa".

---

    Code
      iris %>% filter(1:n())
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1` must be a logical vector, not a integer.
      i Input `..1` is `1:n()`.

---

    Code
      iris %>% group_by(Species) %>% filter(c(TRUE, FALSE))
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1` must be of size 50 or 1, not size 2.
      i Input `..1` is `c(TRUE, FALSE)`.
      i The error occurred in group 1: Species = "setosa".

---

    Code
      iris %>% rowwise(Species) %>% filter(c(TRUE, FALSE))
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1` must be of size 1, not size 2.
      i Input `..1` is `c(TRUE, FALSE)`.
      i The error occurred in row 1.

---

    Code
      iris %>% filter(c(TRUE, FALSE))
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1` must be of size 150 or 1, not size 2.
      i Input `..1` is `c(TRUE, FALSE)`.

---

    Code
      iris %>% group_by(Species) %>% filter(data.frame(c(TRUE, FALSE)))
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1` must be of size 50 or 1, not size 2.
      i Input `..1` is `data.frame(c(TRUE, FALSE))`.
      i The error occurred in group 1: Species = "setosa".

---

    Code
      iris %>% rowwise() %>% filter(data.frame(c(TRUE, FALSE)))
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1` must be of size 1, not size 2.
      i Input `..1` is `data.frame(c(TRUE, FALSE))`.
      i The error occurred in row 1.

---

    Code
      iris %>% filter(data.frame(c(TRUE, FALSE)))
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1` must be of size 150 or 1, not size 2.
      i Input `..1` is `data.frame(c(TRUE, FALSE))`.

---

    Code
      tibble(x = 1) %>% filter(c(TRUE, TRUE))
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1` must be of size 1, not size 2.
      i Input `..1` is `c(TRUE, TRUE)`.

---

    Code
      iris %>% group_by(Species) %>% filter(data.frame(Sepal.Length > 3, 1:n()))
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1$X1.n..` must be a logical vector, not a integer.
      i Input `..1` is `data.frame(Sepal.Length > 3, 1:n())`.
      i The error occurred in group 1: Species = "setosa".

---

    Code
      iris %>% filter(data.frame(Sepal.Length > 3, 1:n()))
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x Input `..1$X1.n..` must be a logical vector, not a integer.
      i Input `..1` is `data.frame(Sepal.Length > 3, 1:n())`.

---

    Code
      mtcars %>% filter(`_x`)
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x object '_x' not found
      i Input `..1` is `_x`.

---

    Code
      mtcars %>% group_by(cyl) %>% filter(`_x`)
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x object '_x' not found
      i Input `..1` is `_x`.
      i The error occurred in group 1: cyl = 4.

---

    Code
      filter(mtcars, x = 1)
    Error <rlang_error>
      Problem with `filter()` input `..1`.
      x Input `..1` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

---

    Code
      filter(mtcars, y > 2, z = 3)
    Error <rlang_error>
      Problem with `filter()` input `..2`.
      x Input `..2` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `z == 3`?

---

    Code
      filter(mtcars, TRUE, x = 1)
    Error <rlang_error>
      Problem with `filter()` input `..2`.
      x Input `..2` is named.
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `x == 1`?

---

    Code
      filter(ts(1:10))
    Error <rlang_error>
      Problem with `filter()` input `.data`.
      x `.data` is a <ts> object, not a data source.
      i Did you want to use `stats::filter()`?

---

    Code
      tibble() %>% filter(stop("{"))
    Error <dplyr_error>
      Problem with `filter()` input `..1`.
      x {
      i Input `..1` is `stop("{")`.

---

    Code
      data.frame(x = 1, y = 1) %>% filter(across(everything(), ~.x > 0))
    Output
        x y
      1 1 1

---

    Code
      data.frame(x = 1, y = 1) %>% filter(data.frame(x > 0, y > 0))
    Output
        x y
      1 1 1

