# selection works with grouped data frames (#2624)

    Code
      out <- mutate_if(gdf, is.factor, as.character)
    Message <message>
      `mutate_if()` ignored the following grouping variables:
      Column `Species`

# colwise mutate gives meaningful error messages

    Can't subset columns that don't exist.
    x Column `test` doesn't exist.

---

    Can't subset columns that don't exist.
    x Column `gr1` doesn't exist.

---

    Problem with `mutate()` input `mpg`.
    x 3 arguments passed to 'length' which requires 1
    i Input `mpg` is `.Primitive("length")(mpg, 0, 0)`.

---

    Problem with `mutate()` input `mpg`.
    x formal argument "na.rm" matched by multiple actual arguments
    i Input `mpg` is `(function (x, ...) ...`.

