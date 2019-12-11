context("colwise arrange")

df <- mtcars[1:3]

test_that("scoped arrange is identical to manual arrange", {
  expect_identical(arrange_all(df), arrange(df, mpg, cyl, disp))
  expect_identical(arrange_at(df, vars(mpg)), arrange(df, mpg))
  expect_identical(arrange_if(iris, is.factor), arrange(iris, Species))
})

test_that(".funs is applied to variables before sorting", {
  expect_identical(arrange_all(df, `-`), arrange(df, -mpg, -cyl, -disp))
})

test_that("arrange_at can arrange by grouping variables (#3351, #3332, #3480)", {
  tbl <- tibble(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8) %>%
    group_by(gr1)

  expect_identical(
    arrange_at(tbl, vars(gr1)),
    arrange(tbl, gr1)
  )

  expect_identical(
    arrange_at(tbl, vars(-x)),
    arrange(tbl, gr1, gr2)
  )
})

test_that("arrange_all arranges by grouping variable (#3351, #3480)", {
  tbl <- tibble(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8) %>%
    group_by(gr1)

  expect_identical(
    arrange_all(tbl),
    arrange(tbl, gr1, gr2, x)
  )

  expect_identical(
    arrange_all(tbl, desc),
    arrange(tbl, desc(gr1), desc(gr2), desc(x))
  )
})

test_that("arrange_if arranges by grouping variable (#3351, #3480)", {
  tbl <- tibble(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8) %>%
    group_by(gr1)

  expect_identical(
    arrange_if(tbl, is.integer),
    arrange(tbl, gr1, gr2, x)
  )
})

test_that("scoped arrange respect .by_group (#3245)",{
  d <- group_by(df, cyl)
  expect_identical(
    arrange_all(d, .by_group = TRUE),
    arrange(d, cyl, mpg, disp)
  )
  expect_identical(
    arrange_if(d, is.numeric, .by_group = TRUE),
    arrange(d, cyl, mpg, disp)
  )
  expect_identical(
    arrange_at(d, vars(mpg, disp), .by_group = TRUE),
    arrange(d, cyl, mpg, disp)
  )
})
