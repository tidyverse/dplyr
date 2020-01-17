test_that(".before and .after relocate individual cols", {
  df <- tibble(x = 1, y = 2)
  expect_named(relocate(df, x, .after = y), c("y", "x"))
  expect_named(relocate(df, y, .before = x), c("y", "x"))
})

test_that("can move blocks of variables", {
  df <- tibble(x = 1, a = "a", y = 2, b = "a")
  expect_named(relocate(df, is.character), c("a", "b", "x", "y"))
  expect_named(relocate(df, is.character, .after = is.numeric), c("x", "y", "a", "b"))
})

test_that("no .before/.after moves to front", {
  df <- tibble(x = 1, y = 2)
  expect_named(relocate(df, y), c("y", "x"))
})

test_that("can only supply one of .before and .after", {
  df <- tibble(x = 1)
  expect_error(relocate(df, .before = 1, .after = 1), "only one")
})
