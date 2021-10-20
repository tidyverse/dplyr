# set operation give useful error message. #903

    Code
      alfa <- tibble(land = c("Sverige", "Norway", "Danmark", "Island", "GB"), data = rnorm(
        length(land)))
      beta <- tibble(land = c("Norge", "Danmark", "Island", "Storbritannien"), data2 = rnorm(
        length(land)))
      (expect_error(intersect(alfa, beta)))
    Output
      <error/rlang_error>
      Error in `intersect()`: `x` and `y` are not compatible: 
      not compatible: 
      - Cols in y but not x: `data2`.
      - Cols in x but not y: `data`.
    Code
      (expect_error(union(alfa, beta)))
    Output
      <error/rlang_error>
      Error in `union()`: `x` and `y` are not compatible: 
      not compatible: 
      - Cols in y but not x: `data2`.
      - Cols in x but not y: `data`.
    Code
      (expect_error(setdiff(alfa, beta)))
    Output
      <error/rlang_error>
      Error in `setdiff()`: `x` and `y` are not compatible: 
      not compatible: 
      - Cols in y but not x: `data2`.
      - Cols in x but not y: `data`.

