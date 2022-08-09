# ------------------------------------------------------------------------------
# relocate()

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

  expect_snapshot(error = TRUE, {
    relocate(df, .before = 1, .after = 1)
  })
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

test_that("relocate() can rename (#5569)", {
  df <- tibble(a = 1, b = 1, c = 1, d = "a", e = "a", f = "a")
  expect_equal(
    relocate(df, ffff = f),
    tibble(ffff = "a", a = 1, b = 1, c = 1, d = "a", e = "a")
  )
  expect_equal(
    relocate(df, ffff = f, .before = c),
    tibble(a = 1, b = 1, ffff = "a", c = 1, d = "a", e = "a")
  )
  expect_equal(
    relocate(df, ffff = f, .after = c),
    tibble(a = 1, b = 1, c = 1, ffff = "a", d = "a", e = "a")
  )
})

test_that("`relocate()` retains the last duplicate when renaming while moving (#6209)", {
  # To enforce the invariant that `ncol(.data) == ncol(relocate(.data, ...))`.
  # Also matches `rename()` behavior.

  df <- tibble(x = 1)

  expect_named(relocate(df, a = x, b = x), "b")
  expect_identical(
    relocate(df, a = x, b = x),
    rename(df, a = x, b = x)
  )

  df <- tibble(x = 1, y = 2)

  expect_named(relocate(df, a = x, b = y, c = x), c("b", "c"))
  expect_identical(
    relocate(df, a = x, b = y, c = x),
    select(rename(df, a = x, b = y, c = x), b, c)
  )
})

test_that("attributes of bare data frames are retained (#6341)", {
  # We require `[` methods to be in charge of keeping extra attributes for all
  # data frame subclasses (except for data.tables)
  df <- vctrs::data_frame(x = 1, y = 2)
  attr(df, "foo") <- "bar"

  out <- relocate(df, y, .before = x)

  expect_identical(attr(out, "foo"), "bar")
})

# ------------------------------------------------------------------------------
# eval_relocate()

test_that("works with zero column data frames (#6167)", {
  data <- tibble()
  expr <- expr(any_of("b"))

  expect_identical(
    eval_relocate(expr, data),
    set_names(integer())
  )
})

test_that("works with `before` and `after` `everything()`", {
  data <- tibble(w = 1, x = 2, y = 3, z = 4)
  expr <- expr(c(y, z))
  expr_everything <- expr(everything())

  expect_identical(
    eval_relocate(expr, data, before = expr_everything),
    c(y = 3L, z = 4L, w = 1L, x = 2L)
  )
  expect_identical(
    eval_relocate(expr, data, after = expr_everything),
    c(w = 1L, x = 2L, y = 3L, z = 4L)
  )
})

test_that("moves columns to the front when neither `before` nor `after` are specified", {
  data <- tibble(x = 1, y = 2, z = 3)
  expr <- expr(c(z, y))

  expect_identical(
    eval_relocate(expr, data),
    c(z = 3L, y = 2L, x = 1L)
  )
})

test_that("Empty `before` selection moves columns to front", {
  data <- tibble(x = 1, y = 2, z = 3)
  expr <- expr(y)
  before <- expr(where(is.character))

  expect_identical(
    eval_relocate(expr, data, before = before),
    c(y = 2L, x = 1L, z = 3L)
  )
})

test_that("Empty `after` selection moves columns to end", {
  data <- tibble(x = 1, y = 2, z = 3)
  expr <- expr(y)
  after <- expr(where(is.character))

  expect_identical(
    eval_relocate(expr, data, after = after),
    c(x = 1L, z = 3L, y = 2L)
  )
})

test_that("Empty `before` and `after` selections work with 0-col data frames", {
  data <- tibble()
  expr <- expr(any_of("a"))
  expr_is_character <- expr(where(is.character))

  expect_identical(
    eval_relocate(expr, data, before = expr_is_character),
    set_names(integer())
  )

  expect_identical(
    eval_relocate(expr, data, after = expr_is_character),
    set_names(integer())
  )
})

test_that("retains the last duplicate when renaming while moving (#6209)", {
  # To enforce the invariant that relocating can't change the number of columns
  data <- tibble(x = 1)
  expr <- expr(c(a = x, b = x))

  expect_identical(
    eval_relocate(expr, data),
    c(b = 1L)
  )

  data <- tibble(x = 1, y = 2)
  expr <- expr(c(a = x, b = y, c = x))

  expect_identical(
    eval_relocate(expr, data),
    c(b = 2L, c = 1L)
  )
})
