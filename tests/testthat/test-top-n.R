context("top_n")

test_that("top_n returns n rows", {
  test_df <- data.frame(x = 1:10, y = 11:20)
  top_four <- test_df %>% top_n(4, y)
  expect_equal(dim(top_four), c(4, 2))
})

test_that("top_n() handles missing `wt`", {
  df <- data.frame(x = c(10, 4, 1, 6, 3, 1, 1))
  expect_message(
    regexp = "Selecting by x",
    expect_identical(top_n(df, 2)$x, c(10, 6))
  )
})

test_that("top_n() handles calls", {
  expect_identical(top_n(mtcars, 2, -disp), top_n(mtcars, -2, disp))
})

test_that("top_n() quotes n", {
  expect_identical(top_n(mtcars, n() * .5), top_n(mtcars, 16))
})

test_that("top_frac() is a shorthand for top_n(n()*)", {
  expect_identical(top_n(mtcars, n() * .5, disp), top_frac(mtcars, .5, disp))

  expect_message(
    regexp = "Selecting by carb",
    expect_identical(top_n(mtcars, n() * .5), top_frac(mtcars, .5))
  )
})
