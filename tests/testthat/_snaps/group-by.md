# select(group_by(.)) implicitely adds grouping variables (#170)

    Code
      res <- mtcars %>% group_by(vs) %>% select(mpg)
    Message <rlang_message>
      Adding missing grouping variables: `vs`

# group_by works with zero-row data frames (#486)

    Code
      x <- select(dfg, a)
    Message <rlang_message>
      Adding missing grouping variables: `g`

# group_by() and ungroup() give meaningful error messages

    Code
      df <- tibble(x = 1, y = 2)
      (expect_error(df %>% group_by(unknown)))
    Output
      <error/rlang_error>
      Error in `group_by_prepare()`: Must group by variables found in `.data`.
      x Column `unknown` is not found.
    Code
      (expect_error(df %>% ungroup(x)))
    Output
      <error/rlib_error_dots_nonempty>
      Error in `ungroup.data.frame()`: `...` is not empty.
      i These dots only exist to allow future extensions and should be empty.
      x We detected these problematic arguments:
      * `..1`
      i Did you misspecify an argument?
    Code
      (expect_error(df %>% group_by(x, y) %>% ungroup(z)))
    Output
      <error/vctrs_error_subscript_oob>
      Error in `ungroup()`: Can't subset columns that don't exist.
      x Column `z` doesn't exist.
    Code
      (expect_error(df %>% group_by(z = a + 1)))
    Output
      <error/rlang_error>
      Error in `group_by()`: 
        Problem adding computed columns.
      Caused by error in `mutate()`: 
        Problem while computing `z = a + 1`.
      Caused by error: 
        object 'a' not found

