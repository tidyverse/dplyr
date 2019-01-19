context("colwise distinct")

test_that("scoped distinct is identical to manual distinct", {
  df <- tibble(
    x = rep(2:5, each=2),
    y = rep(2:3, each = 4),
    z = "a"
  )

  expect_identical(distinct_all(df), distinct(df, x, y, z))
  expect_identical(distinct_at(df, vars(x)), distinct(df, x))
  expect_identical(distinct_if(df, is.integer), distinct(df, x, y))
})

test_that(".funs is applied to variables before getting distinct rows", {
  df <- tibble(
    x = rep(2:5, each=2),
    y = rep(2:3, each = 4)
  )

  expect_identical(distinct_all(df, `-`), distinct(mutate_all(df,`-`), x, y))
})

test_that("scoped distinct applies to grouping variables (#3480)", {
  df <- tibble(
    g = rep(1:2, each = 4),
    x = rep(2:5, each = 2) / 2,
    y = rep(2:3, each = 4) / 2
  )
  out <- df[c(1, 3, 5, 8), ]

  expect_identical(distinct_all(df), out)
  expect_identical(distinct_at(df, vars(g, x, y)), out)
  expect_identical(distinct_if(df, is.numeric), out)
})
