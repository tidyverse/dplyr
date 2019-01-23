context("Distinct")

test_that("distinct equivalent to local unique when keep_all is TRUE", {
  df <- data.frame(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2)
  )

  expect_equal(distinct(df), unique(df))
})

test_that("distinct for single column works as expected (#1937)", {
  df <- tibble(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2)
  )

  expect_equal(distinct(df, x, .keep_all = FALSE), unique(df["x"]))
  expect_equal(distinct(df, y, .keep_all = FALSE), unique(df["y"]))
})

test_that("distinct works for 0-sized columns (#1437)", {
  df <- tibble(x = 1:10) %>% select(-x)
  ddf <- distinct(df)
  expect_equal(ncol(ddf), 0L)
})

test_that("if no variables specified, uses all", {
  df <- tibble(x = c(1, 1), y = c(2, 2))
  expect_equal(distinct(df), tibble(x = 1, y = 2))
})

test_that("distinct keeps only specified cols", {
  df <- tibble(x = c(1, 1, 1), y = c(1, 1, 1))
  expect_equal(df %>% distinct(x), tibble(x = 1))
})

test_that("unless .keep_all = TRUE", {
  df <- tibble(x = c(1, 1, 1), y = 3:1)

  expect_equal(df %>% distinct(x), tibble(x = 1))
  expect_equal(df %>% distinct(x, .keep_all = TRUE), tibble(x = 1, y = 3L))
})

test_that("distinct doesn't duplicate columns", {
  df <- tibble(a = 1:3, b = 4:6)

  expect_named(df %>% distinct(a, a), "a")
  expect_named(df %>% group_by(a) %>% distinct(a), "a")
})


test_that("grouped distinct always includes group cols", {
  df <- tibble(g = c(1, 2), x = c(1, 2))

  out <- df %>% group_by(g) %>% distinct(x)
  expect_equal(df, out)
})

test_that("empty grouped distinct equivalent to empty ungrouped", {
  df <- tibble(g = c(1, 2), x = c(1, 2))

  df1 <- df %>% distinct() %>% group_by(g)
  df2 <- df %>% group_by(g) %>% distinct()

  expect_equal(df1, df2)
})

test_that("distinct gives a warning when selecting an unknown column (#3140)", {
  df <- tibble(g = c(1, 2), x = c(1, 2))

  expect_warning(
    distinct(df, aa),
    glue("Trying to compute distinct() for variables not found in the data:
         - `aa`
         This is an error, but only a warning is raised for compatibility reasons.
         The operation will return the input unchanged."),
    fixed = TRUE
  )
  expect_warning(
    distinct(df, .data$aa),
    glue("Trying to compute distinct() for variables not found in the data:
      - `aa`
      This is an error, but only a warning is raised for compatibility reasons.
      The operation will return the input unchanged."),
    fixed = TRUE
    )


  expect_warning(
    distinct(df, aa, x),
    glue("Trying to compute distinct() for variables not found in the data:
         - `aa`
         This is an error, but only a warning is raised for compatibility reasons.
         The following variables will be used:
         - x"),
    fixed = TRUE
  )
  expect_warning(
    distinct(df, .data$aa, x),
    glue("Trying to compute distinct() for variables not found in the data:
      - `aa`
      This is an error, but only a warning is raised for compatibility reasons.
      The following variables will be used:
      - x"),
    fixed = TRUE
    )

  expect_warning(
    distinct(df, g, aa, x),
    glue("Trying to compute distinct() for variables not found in the data:
         - `aa`
         This is an error, but only a warning is raised for compatibility reasons.
         The following variables will be used:
         - g
         - x"),
    fixed = TRUE
  )
  expect_warning(
    distinct(df, g, .data$aa, x),
    glue("Trying to compute distinct() for variables not found in the data:
      - `aa`
      This is an error, but only a warning is raised for compatibility reasons.
      The following variables will be used:
      - g
      - x"),
    fixed = TRUE
    )
})

test_that("distinct on a new, mutated variable is equivalent to mutate followed by distinct", {
  df <- tibble(g = c(1, 2), x = c(1, 2))

  df1 <- df %>% distinct(aa = g * 2)
  df2 <- df %>% mutate(aa = g * 2) %>% distinct(aa)

  expect_equal(df1, df2)
})

test_that("distinct on a new, copied variable is equivalent to mutate followed by distinct (#3234)", {
  df <- tibble(g = c(1, 2), x = c(1, 2))

  df1 <- df %>% distinct(aa = g)
  df2 <- df %>% mutate(aa = g) %>% distinct(aa)

  expect_equal(df1, df2)
})

test_that("distinct on a dataframe or tibble with columns of type list throws an error", {
  df <- tibble(
    a = c("1", "1", "2", "2", "3", "3"),
    b = list("A")
  )
  df2 <- data.frame(x = 1:5, y = I(list(1:3, 2:4, 3:5, 4:6, 5:7)))

  expect_warning(
    expect_identical(df %>% distinct(), df %>% slice(c(1, 3, 5))),
    "distinct() does not fully support columns of type `list`.\nList elements are compared by reference, see ?distinct for details.\nThis affects the following columns:\n- `b`",
    fixed = TRUE
  )
  expect_warning(
    expect_identical(df2 %>% distinct(), df2),
    "distinct() does not fully support columns of type `list`.\nList elements are compared by reference, see ?distinct for details.\nThis affects the following columns:\n- `y`",
    fixed = TRUE
  )
})

test_that("distinct deals with Period and Interval from lubridate (#2568)", {
  skip_if_not(requireNamespace("lubridate", quietly = TRUE))

  df <- tibble(
    x = lubridate::hm("10:30", "10:30", "0:0"),
    y = c("apple", "apple", "tomato")
  )
  res <- distinct(df)
  expect_equal(res, df[c(1, 3), ])

  df <- tibble(
    lubridate::interval(lubridate::ymd(20090201), lubridate::ymd(20090101))
  )
  expect_equal(df, distinct(df))
})

test_that("distinct handles 0 columns edge case (#2954)", {
  d <- select(data.frame(x= c(1, 1)), one_of(character(0)))
  res <- distinct(d)
  expect_equal(nrow(res), 1L)

  expect_equal(nrow(distinct(tibble())), 0L)
})

test_that("distinct respects the order of the given variables (#3195)",{
  d <- data.frame(x=1:2, y=3:4)
  expect_equal(names(distinct(d, y, x)), c("y", "x"))
})
