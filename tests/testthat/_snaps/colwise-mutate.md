# selection works with grouped data frames (#2624)

    Code
      out <- mutate_if(gdf, is.factor, as.character)
    Message
      `mutate_if()` ignored the following grouping variables:
      * Column `Species`

# colwise mutate gives meaningful error messages

    Code
      (expect_error(mutate_at(tibble(), "test", ~1)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `mutate_at()`:
      ! Can't subset columns that don't exist.
      x Column `test` doesn't exist.
    Code
      tbl <- tibble(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8)
      tbl <- group_by(tbl, gr1)
      (expect_error(summarise_at(tbl, vars(gr1), mean)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `summarise_at()`:
      ! Can't subset columns that don't exist.
      x Column `gr1` doesn't exist.
    Code
      (expect_error(mutate_all(mtcars, length, 0, 0)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `mpg = .Primitive("length")(mpg, 0, 0)`.
      Caused by error:
      ! 3 arguments passed to 'length' which requires 1
    Code
      (expect_error(mutate_all(mtcars, mean, na.rm = TRUE, na.rm = TRUE)))
    Output
      <error/dplyr:::mutate_error>
      Error in `mutate()`:
      ! Problem while computing `mpg = (function (x, ...) ...`.
      Caused by error in `mean.default()`:
      ! formal argument "na.rm" matched by multiple actual arguments

