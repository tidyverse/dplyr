context("Copying")

test_that("coercion doesn't copy vars", {
  mtcars2 <- tbl_df(mtcars)
  mtcars3 <- as.data.frame(mtcars2)

  expect_equal(location(mtcars2)$vars, location(mtcars)$vars)
  expect_equal(location(mtcars3)$vars, location(mtcars)$vars)
})

test_that("grouping and ungrouping doesn't copy vars", {
  mtcars2 <- group_by(mtcars, cyl)
  mtcars3 <- ungroup(mtcars2)

  expect_equal(location(mtcars2)$vars, location(mtcars)$vars)
  expect_equal(location(mtcars3)$vars, location(mtcars)$vars)
})

test_that("mutate doesn't copy vars", {
  mtcars2 <- tbl_df(mtcars)
  mtcars3 <- mutate(mtcars2, cyl2 = cyl * 2)

  expect_equal(location(mtcars3)$vars[1:11], location(mtcars2)$vars)
})

test_that("select doesn't copy vars", {
  mtcars2 <- tbl_df(mtcars)
  mtcars3 <- select(mtcars2, carb:mpg)

  expect_equal(location(mtcars3)$vars[11:1], location(mtcars2)$vars)
})
