# select(group_by(.)) implicitely adds grouping variables (#170)

    Code
      res <- mtcars %>% group_by(vs) %>% select(mpg)
    Message <message>
      Adding missing grouping variables: `vs`

# group_by works with zero-row data frames (#486)

    Code
      x <- select(dfg, a)
    Message <message>
      Adding missing grouping variables: `g`

# group_by() and ungroup() give meaningful error messages

    Code
      df %>% group_by(unknown)
    Error <rlang_error>
      Must group by variables found in `.data`.
      * Column `unknown` is not found.

---

    Code
      df %>% ungroup(x)
    Error <rlib_error_dots_nonempty>
      `...` is not empty.
      
      We detected these problematic arguments:
      * `..1`
      
      These dots only exist to allow future extensions and should be empty.
      Did you misspecify an argument?

---

    Code
      df %>% group_by(x, y) %>% ungroup(z)
    Error <vctrs_error_subscript_oob>
      Can't subset columns that don't exist.
      x Column `z` doesn't exist.

---

    Code
      df %>% group_by(z = a + 1)
    Error <rlang_error>
      Problem adding computed columns in `group_by()`.
      x Problem with `mutate()` column `z`.
      x object 'a' not found
      i Input `z` is `a + 1`.

