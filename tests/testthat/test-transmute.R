context("transmute")

test_that("non-syntactic grouping variable is preserved (#1138)", {
  df <- tibble(`a b` = 1L) %>% group_by(`a b`) %>% transmute()
  expect_named(df, "a b")
})

test_that("transmute preserves variable order", {
  df <- tibble(x = 1, g = 2)

  out <- df %>%
    group_by(g) %>%
    transmute(x = 2)

  expect_named(out, c("x", "g"))
})

# Empty transmutes -------------------------------------------------

test_that("transmute with no args returns nothing", {
  empty <- transmute(mtcars)
  expect_equal(ncol(empty), 0)
  expect_equal(nrow(empty), 32)
})

# transmute variables -----------------------------------------------

test_that("transmute succeeds in presence of raw columns (#1803)", {
  df <- tibble(a = 1:3, b = as.raw(1:3))
  expect_identical(transmute(df, a), df["a"])
  expect_identical(transmute(df, b), df["b"])
})

test_that("arguments to transmute() don't match vars_transmute() arguments", {
  df <- tibble(a = 1)
  expect_identical(transmute(df, var = a), tibble(var = 1))
  expect_identical(transmute(df, exclude = a), tibble(exclude = 1))
  expect_identical(transmute(df, include = a), tibble(include = 1))
})

test_that("arguments to rename() don't match vars_rename() arguments (#2861)", {
  df <- tibble(a = 1)
  expect_identical(rename(df, var = a), tibble(var = 1))
  expect_identical(rename(group_by(df, a), var = a), group_by(tibble(var = 1), var))
  expect_identical(rename(df, strict = a), tibble(strict = 1))
  expect_identical(rename(group_by(df, a), strict = a), group_by(tibble(strict = 1), strict))
})

test_that("can transmute() with .data pronoun (#2715)", {
  expect_identical(transmute(mtcars, .data$cyl), transmute(mtcars, cyl))
})

test_that("transmute() does not warn when a variable is removed with = NULL (#4609)", {
  df <- data.frame(x=1)
  expect_warning(transmute(df, y =x+1, z=y*2, y = NULL), NA)
})

test_that("transmute() can handle auto splicing", {
  expect_equal(
    iris %>% transmute(tibble(Sepal.Length, Sepal.Width)),
    iris %>% select(Sepal.Length, Sepal.Width)
  )
})
