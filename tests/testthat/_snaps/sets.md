# set operation give useful error message. #903

    Code
      alfa <- tibble(land = c("Sverige", "Norway", "Danmark", "Island", "GB"), data = rnorm(
        length(land)))
      beta <- tibble(land = c("Norge", "Danmark", "Island", "Storbritannien"), data2 = rnorm(
        length(land)))
      gamma <- tibble(land = 1:2, data = 1:2)
      (expect_error(intersect(alfa, beta)))
    Output
      <error/rlang_error>
      Error in `intersect()`:
      ! `x` and `y` are not compatible.
      x Cols in `y` but not `x`: `data2`.
      x Cols in `x` but not `y`: `data`.
    Code
      (expect_error(intersect(alfa, 1)))
    Output
      <error/rlang_error>
      Error in `intersect()`:
      ! `y` must be a data frame.
    Code
      (expect_error(intersect(alfa, gamma)))
    Output
      <error/rlang_error>
      Error in `intersect()`:
      ! `x` and `y` are not compatible.
      x Incompatible types for column `land`: character vs integer.
    Code
      (expect_error(union(alfa, beta)))
    Output
      <error/rlang_error>
      Error in `union()`:
      ! `x` and `y` are not compatible.
      x Cols in `y` but not `x`: `data2`.
      x Cols in `x` but not `y`: `data`.
    Code
      (expect_error(setdiff(alfa, beta)))
    Output
      <error/rlang_error>
      Error in `setdiff()`:
      ! `x` and `y` are not compatible.
      x Cols in `y` but not `x`: `data2`.
      x Cols in `x` but not `y`: `data`.

