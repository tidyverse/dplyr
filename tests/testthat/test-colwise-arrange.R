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
