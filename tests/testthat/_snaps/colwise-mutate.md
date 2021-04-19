# selection works with grouped data frames (#2624)

    Code
      out <- mutate_if(gdf, is.factor, as.character)
    Message <message>
      `mutate_if()` ignored the following grouping variables:
      Column `Species`

# colwise mutate gives meaningful error messages

    Code
      mutate_at(tibble(), "test", ~1)
    Error <vctrs_error_subscript_oob>
      Can't subset columns that don't exist.
      x Column `test` doesn't exist.

---

    Code
      summarise_at(tbl, vars(gr1), mean)
    Error <vctrs_error_subscript_oob>
      Can't subset columns that don't exist.
      x Column `gr1` doesn't exist.

---

    Code
      mutate_all(mtcars, length, 0, 0)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `mpg`.
      i `mpg = .Primitive("length")(mpg, 0, 0)`.
      x 3 arguments passed to 'length' which requires 1

---

    Code
      mutate_all(mtcars, mean, na.rm = TRUE, na.rm = TRUE)
    Error <dplyr:::mutate_error>
      Problem with `mutate()` column `mpg`.
      i `mpg = (function (x, ...) ...`.
      x formal argument "na.rm" matched by multiple actual arguments

