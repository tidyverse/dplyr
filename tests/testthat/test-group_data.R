context("group_data")

test_that("group_rows works for 3 most important subclasses (#3489)", {
  df <- data.frame(x=c(1,1,2,2))
  expect_equal(group_rows(df), list(0:3))
  expect_equal(group_rows(group_by(df,x)), list(0:1, 2:3))
  expect_equal(group_rows(rowwise(df)), as.list(0:3))
})

test_that("group_data returns a tidy tibble (#3489)", {
  df <- tibble(x = c(1,1,2,2))

  expect_identical(
    group_data(df),
    tibble(.rows=list(0:3))
  )

  expect_identical(
    group_by(df,x) %>% group_data(),
    tibble(x = c(1,2), .rows = list(0:1, 2:3))
  )

  expect_identical(
    rowwise(df) %>% group_data(),
    tibble(.rows = as.list(0:3))
  )
})

test_that("group_rows and group_data work with 0 rows data frames (#3489)", {
  df <- tibble(x=integer())
  expect_identical(group_rows(df), list(integer()))
  expect_identical(group_rows(rowwise(df)), list())
  expect_identical(group_rows(group_by(df, x)), list(integer()))

  expect_identical(group_data(df), tibble(.rows = list(integer())))
  expect_identical(group_data(rowwise(df)), tibble(.rows =list()))
  expect_identical(group_data(group_by(df, x)), tibble(x = NA_integer_, .rows = list(integer())))
})
