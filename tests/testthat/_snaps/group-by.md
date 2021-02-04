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

    Must group by variables found in `.data`.
    * Column `unknown` is not found.

---

    `...` is not empty.
    
    We detected these problematic arguments:
    * `..1`
    
    These dots only exist to allow future extensions and should be empty.
    Did you misspecify an argument?

---

    Can't subset columns that don't exist.
    x Column `z` doesn't exist.

---

    Problem adding computed columns in `group_by()`.
    x Problem with `mutate()` input `z`.
    x object 'a' not found
    i Input `z` is `a + 1`.

