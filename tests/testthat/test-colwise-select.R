context("colwise select")

df <- data_frame(x = 0L, y = 0.5, z = 1)

test_that("can select/rename all variables", {
  expect_identical(select_all(df), df)
  expect_error(
    rename_all(df),
    "`.funs` must specify a renaming function",
    fixed = TRUE
  )

  expect_identical(select_all(df, toupper), set_names(df, c("X", "Y", "Z")))
  expect_identical(select_all(df, toupper), rename_all(df, toupper))
})

test_that("can select/rename with predicate", {
  expect_identical(select_if(df, is_integerish), select(df, x, z))
  expect_error(
    rename_if(df, is_integerish),
    "`.funs` must specify a renaming function",
    fixed = TRUE
  )

  expect_identical(select_if(df, is_integerish, toupper), set_names(df[c("x", "z")], c("X", "Z")))
  expect_identical(rename_if(df, is_integerish, toupper), set_names(df, c("X", "y", "Z")))
})

test_that("can supply funs()", {
  expect_identical(select_if(df, funs(is_integerish(.)), funs(toupper(.))), set_names(df[c("x", "z")], c("X", "Z")))
  expect_identical(rename_if(df, funs(is_integerish(.)), funs(toupper(.))), set_names(df, c("X", "y", "Z")))
})

test_that("fails when more than one renaming function is supplied", {
  expect_error(
    select_all(df, funs(tolower, toupper)),
    "`.funs` must contain one renaming function, not 2",
    fixed = TRUE
  )
  expect_error(
    rename_all(df, funs(tolower, toupper)),
    "`.funs` must contain one renaming function, not 2",
    fixed = TRUE
  )
})

test_that("can select/rename with vars()", {
  expect_identical(select_at(df, vars(x:y)), df[-3])
  expect_error(
    rename_at(df, vars(x:y)),
    "`.funs` must specify a renaming function",
    fixed = TRUE
  )

  expect_identical(select_at(df, vars(x:y), toupper), set_names(df[-3], c("X", "Y")))
  expect_identical(rename_at(df, vars(x:y), toupper), set_names(df, c("X", "Y", "z")))
})

test_that("select_if keeps grouping cols", {
  expect_silent(df <- iris %>% group_by(Species) %>% select_if(is.numeric))
  expect_equal(df, tbl_df(iris[c(5, 1:4)]))
})

test_that("select_if() handles non-syntactic colnames", {
  df <- data_frame(`x 1` = 1:3)
  expect_identical(select_if(df, is_integer)[[1]], 1:3)
})

test_that("select_if() handles quoted predicates", {
  expected <- select_if(mtcars, is_integerish)
  expect_identical(select_if(mtcars, "is_integerish"), expected)
  expect_identical(select_if(mtcars, ~is_integerish(.x)), expected)
})
