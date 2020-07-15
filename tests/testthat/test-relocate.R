test_that(".before and .after relocate individual cols", {
  df <- tibble(x = 1, y = 2)
  expect_named(relocate(df, x, .after = y), c("y", "x"))
  expect_named(relocate(df, y, .before = x), c("y", "x"))
})

test_that("can move blocks of variables", {
  df <- tibble(x = 1, a = "a", y = 2, b = "a")
  expect_named(relocate(df, where(is.character)), c("a", "b", "x", "y"))
  expect_named(relocate(df, where(is.character), .after = where(is.numeric)), c("x", "y", "a", "b"))
})

test_that("don't lose non-contiguous variables", {
  df <- tibble(a = 1, b = 1, c = 1, d = 1, e = 1)
  expect_named(relocate(df, b, .after = c(a, c, e)), c("a", "c", "d", "e", "b"))
  expect_named(relocate(df, e, .before = c(b, d)), c("a", "e", "b", "c", "d"))
})

test_that("no .before/.after moves to front", {
  df <- tibble(x = 1, y = 2)
  expect_named(relocate(df, y), c("y", "x"))
})

test_that("can only supply one of .before and .after", {
  df <- tibble(x = 1)
  expect_error(relocate(df, .before = 1, .after = 1), "only one")
})

test_that("before and after are defused with context", {
  local_fn <- identity
  expect_identical(
    names(relocate(mtcars, 3, .before = local_fn(5))),
    names(relocate(mtcars, 3, .before = 5))
  )
  expect_identical(
    names(relocate(mtcars, 3, .after = local_fn(5))),
    names(relocate(mtcars, 3, .after = 5))
  )
})

test_that("relocate() respects order specified by ... (#5328)", {
  df <- tibble(a = 1, x = 1, b = 1, z = 1, y = 1)

  expect_equal(
    names(relocate(df, x, y, z, .before = x)),
    c("a", "x", "y", "z", "b")
  )
  expect_equal(
    names(relocate(df, x, y, z, .after = last_col())),
    c("a", "b", "x", "y", "z")
  )
  expect_equal(
    names(relocate(df, x, a, z)),
    c("x", "a", "z", "b", "y")
  )
})
