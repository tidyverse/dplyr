test_that("all_equal is deprecated", {
  expect_snapshot(all_equal(mtcars, mtcars))
})

# A data frame with all major types
df_all <- data.frame(
  a = c(1, 2.5),
  b = 1:2,
  c = c(T, F),
  d = c("a", "b"),
  e = factor(c("a", "b")),
  f = Sys.Date() + 1:2,
  g = Sys.time() + 1:2,
  stringsAsFactors = FALSE
)

test_that("data frames equal to themselves", {
  local_options(lifecycle_verbosity = "quiet")

  expect_true(all_equal(mtcars, mtcars))
  expect_true(all_equal(iris, iris))
  expect_true(all_equal(df_all, df_all))
})

test_that("data frames not equal if missing row", {
  local_options(lifecycle_verbosity = "quiet")
  expect_snapshot({
    all_equal(mtcars, mtcars[-1, ])
    all_equal(iris, iris[-1, ])
    all_equal(df_all, df_all[-1, ])
  })
})

test_that("data frames not equal if missing col", {
  local_options(lifecycle_verbosity = "quiet")
  expect_snapshot({
    all_equal(mtcars, mtcars[, -1])
    all_equal(iris, iris[, -1])
    all_equal(df_all, df_all[, -1])
  })
})

test_that("factors equal only if levels equal", {
  local_options(lifecycle_verbosity = "quiet")
  df1 <- tibble(x = factor(c("a", "b")))
  df2 <- tibble(x = factor(c("a", "d")))
  expect_snapshot({
    all_equal(df1, df2)
    all_equal(df2, df1)
  })
})

test_that("factor comparison requires strict equality of levels (#2440)", {
  local_options(lifecycle_verbosity = "quiet")

  df1 <- tibble(x = factor("a"))
  df2 <- tibble(x = factor("a", levels = c("a", "b")))
  expect_true(all_equal(df1, df2, convert = TRUE))
  expect_true(all_equal(df2, df1, convert = TRUE))

  expect_snapshot({
    all_equal(df1, df2)
    all_equal(df2, df1)
  })
})

test_that("all.equal.data.frame handles data.frames with NULL names", {
  local_options(lifecycle_verbosity = "quiet")

  x <- data.frame(LETTERS[1:3], rnorm(3))
  names(x) <- NULL
  suppressMessages(
    expect_true(all_equal(x, x))
  )
})

test_that("data frame equality test with ignore_row_order=TRUE detects difference in number of rows. #1065", {
  local_options(lifecycle_verbosity = "quiet")

  DF1 <- tibble(a = 1:4, b = letters[1:4])
  DF2 <- tibble(a = c(1:4, 4L), b = letters[c(1:4, 4L)])
  expect_false(isTRUE(all_equal(DF1, DF2, ignore_row_order = TRUE)))

  DF1 <- tibble(a = c(1:4, 2L), b = letters[c(1:4, 2L)])
  DF2 <- tibble(a = c(1:4, 4L), b = letters[c(1:4, 4L)])
  expect_false(isTRUE(all_equal(DF1, DF2, ignore_row_order = TRUE)))
})

test_that("all.equal handles NA_character_ correctly. #1095", {
  local_options(lifecycle_verbosity = "quiet")

  d1 <- tibble(x = c(NA_character_))
  expect_true(all_equal(d1, d1))

  d2 <- tibble(x = c(NA_character_, "foo", "bar"))
  expect_true(all_equal(d2, d2))
})

test_that("handle Date columns of different types, integer and numeric (#1204)", {
  local_options(lifecycle_verbosity = "quiet")

  a <- data.frame(date = as.Date("2015-06-07"))
  b <- data.frame(date = structure(as.integer(a$date), class = "Date"))
  expect_true(all_equal(a, b))
})

test_that("equality test fails when convert is FALSE and types don't match (#1484)", {
  local_options(lifecycle_verbosity = "quiet")

  df1 <- tibble(x = "a")
  df2 <- tibble(x = factor("a"))
  expect_true(all_equal(df1, df2, convert = TRUE))

  expect_snapshot({
    all_equal(df1, df2, convert = FALSE)
  })
})

test_that("equality handles data frames with 0 rows (#1506)", {
  df0 <- tibble(x = numeric(0), y = character(0))
  expect_equal(df0, df0)
})

test_that("equality handles data frames with 0 columns (#1506)", {
  df0 <- tibble(a = 1:10)[-1]
  expect_equal(df0, df0)
})

test_that("equality handle raw columns", {
  local_options(lifecycle_verbosity = "quiet")

  df <- tibble(a = 1:3, b = as.raw(1:3))
  expect_true(all_equal(df, df))
})

test_that("equality returns a message for convert = TRUE", {
  local_options(lifecycle_verbosity = "quiet")

  df1 <- tibble(x = 1:3)
  df2 <- tibble(x = as.character(1:3))

  expect_snapshot({
    all_equal(df1, df2)
    all_equal(df1, df2, convert = TRUE)
  })
})

test_that("numeric and integer can be compared if convert = TRUE", {
  local_options(lifecycle_verbosity = "quiet")

  df1 <- tibble(x = 1:3)
  df2 <- tibble(x = as.numeric(1:3))
  expect_true(all_equal(df1, df2, convert = TRUE))

  expect_snapshot({
    all_equal(df1, df2)
  })
})

test_that("returns vector for more than one difference (#1819)", {
  local_options(lifecycle_verbosity = "quiet")

  expect_snapshot({
    all_equal(tibble(a = 1, b = 2), tibble(a = 1L, b = 2L))
  })
})

test_that("ignore column order", {
  local_options(lifecycle_verbosity = "quiet")

  expect_snapshot({
    all_equal(
      tibble(a = 1, b = 2),
      tibble(b = 2, a = 1),
      ignore_col_order = FALSE
    )
    all_equal(tibble(a = 1, b = 2), tibble(a = 1), ignore_col_order = FALSE)
  })
})

# Errors ------------------------------------------------------------------

test_that("count() give meaningful errors", {
  local_options(lifecycle_verbosity = "quiet")

  expect_snapshot({
    (expect_error(union(tibble(a = 1), tibble(a = "1"))))
    (expect_error(union(tibble(a = 1, b = 2), tibble(a = "1", b = "2"))))
  })
})
