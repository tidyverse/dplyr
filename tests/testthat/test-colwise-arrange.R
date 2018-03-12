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

test_that("arrange_at can arrange by grouping variables (#3351)", {
  tbl <- data_frame(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8) %>%
    group_by(gr1)

  expect_identical(
    arrange_at(tbl, vars(gr1)),
    arrange(tbl, gr1)
  )
})
