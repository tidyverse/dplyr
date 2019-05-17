context("Equality")

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
  expect_true(all.equal(tbl_df(mtcars), tbl_df(mtcars)))
  expect_true(all.equal(tbl_df(iris), tbl_df(iris)))
  expect_true(all.equal(tbl_df(df_all), tbl_df(df_all)))
})

test_that("data frames equal to random permutations of themselves", {
  scramble <- function(x) {
    x[sample(nrow(x)), sample(ncol(x)), drop = FALSE]
  }

  expect_equal(tbl_df(mtcars), tbl_df(scramble(mtcars)))
  expect_equal(tbl_df(iris), tbl_df(scramble(iris)))
  expect_equal(tbl_df(df_all), tbl_df(scramble(df_all)))
})

test_that("data frames not equal if missing row", {
  expect_match(all.equal(tbl_df(mtcars), mtcars[-1, ]), "Different number of rows")
  expect_match(all.equal(tbl_df(iris), iris[-1, ]), "Different number of rows")
  expect_match(all.equal(tbl_df(df_all), df_all[-1, ]), "Different number of rows")
})

test_that("data frames not equal if missing col", {
  expect_match(
    all.equal(tbl_df(mtcars), mtcars[, -1]),
    "Cols in x but not y: `mpg`"
  )
  expect_match(
    all.equal(tbl_df(iris), iris[, -1]),
    "Cols in x but not y: `Sepal.Length`"
  )
  expect_match(
    all.equal(tbl_df(df_all), df_all[, -1]),
    "Cols in x but not y: `a`"
  )
})

test_that("factors equal only if levels equal", {
  df1 <- tibble(x = factor(c("a", "b")))
  df2 <- tibble(x = factor(c("a", "d")))
  expect_equal(
    all.equal(df1, df2),
    "Factor levels not equal for column `x`"
  )
  expect_equal(
    all.equal(df2, df1),
    "Factor levels not equal for column `x`"
  )
})

test_that("factor comparison requires strict equality of levels (#2440)", {
  df1 <- tibble(x = factor("a"))
  df2 <- tibble(x = factor("a", levels = c("a", "b")))
  expect_equal(
    all.equal(df1, df2),
    "Factor levels not equal for column `x`"
  )
  expect_equal(
    all.equal(df2, df1),
    "Factor levels not equal for column `x`"
  )
  expect_warning(expect_true(all.equal(df1, df2, convert = TRUE)), "joining factors")
  expect_warning(expect_true(all.equal(df2, df1, convert = TRUE)), "joining factors")
})

test_that("BoolResult does not overwrite singleton R_TrueValue", {
  dplyr:::equal_data_frame(mtcars, mtcars)
  expect_equal(class(2 == 2), "logical")
})

test_that("all.equal.data.frame handles data.frames with NULL names", {
  x <- data.frame(LETTERS[1:3], rnorm(3))
  names(x) <- NULL
  expect_true(all.equal(x, x))
})

test_that("data frame equality test with ignore_row_order=TRUE detects difference in number of rows. #1065", {
  DF1 <- tibble(a = 1:4, b = letters[1:4])
  DF2 <- tibble(a = c(1:4, 4L), b = letters[c(1:4, 4L)])
  expect_false(isTRUE(all.equal(DF1, DF2, ignore_row_order = TRUE)))

  DF1 <- tibble(a = c(1:4, 2L), b = letters[c(1:4, 2L)])
  DF2 <- tibble(a = c(1:4, 4L), b = letters[c(1:4, 4L)])
  expect_false(isTRUE(all.equal(DF1, DF2, ignore_row_order = TRUE)))
})

test_that("all.equal handles NA_character_ correctly. #1095", {
  d1 <- tibble(x = c(NA_character_))
  expect_true(all.equal(d1, d1))

  d2 <- tibble(x = c(NA_character_, "foo", "bar"))
  expect_true(all.equal(d2, d2))
})

test_that("handle Date columns of different types, integer and numeric (#1204)", {
  a <- data.frame(date = as.Date("2015-06-07"))
  b <- data.frame(date = structure(as.integer(a$date), class = "Date"))
  expect_true(all.equal(a, b))
})

test_that("equality test fails when convert is FALSE and types don't match (#1484)", {
  df1 <- tibble(x = "a")
  df2 <- tibble(x = factor("a"))

  expect_equal(
    all_equal(df1, df2, convert = FALSE),
    "Incompatible type for column `x`: x character, y factor"
  )
  expect_warning(all_equal(df1, df2, convert = TRUE))
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
  df <- tibble(a = 1:3, b = as.raw(1:3))
  expect_true(all.equal(df, df))
})

test_that("equality returns a message for convert = TRUE", {
  df1 <- tibble(x = 1:3)
  df2 <- tibble(x = as.character(1:3))
  expect_match(all.equal(df1, df2), "Incompatible")
  expect_match(all.equal(df1, df2, convert = TRUE), "Incompatible")
})

test_that("numeric and integer can be compared if convert = TRUE", {
  df1 <- tibble(x = 1:3)
  df2 <- tibble(x = as.numeric(1:3))
  expect_match(all.equal(df1, df2), "Incompatible")
  expect_true(all.equal(df1, df2, convert = TRUE))
})

test_that("returns vector for more than one difference (#1819)", {
  expect_equal(
    all.equal(tibble(a = 1, b = 2), tibble(a = 1L, b = 2L)),
    c(
      "Incompatible type for column `a`: x numeric, y integer",
      "Incompatible type for column `b`: x numeric, y integer"
    )
  )
})

test_that("returns UTF-8 column names (#2441)", {
  skip_on_cran()
  df1 <- tibble("\u5e78" := 1)
  df2 <- tibble("\u798f" := 1)

  expect_equal(
    all.equal(df1, df2),
    c(
      "Cols in y but not x: `\u798f`. ",
      "Cols in x but not y: `\u5e78`. "
    ),
    fixed = TRUE
  )
})

test_that("proper message formatting for set operations", {
  expect_error(
    union(tibble(a = 1), tibble(a = "1")),
    "not compatible: Incompatible type for column `a`: x numeric, y character",
    fixed = TRUE
  )

  expect_error(
    union(tibble(a = 1, b = 2), tibble(a = "1", b = "2")),
    "not compatible: \n- Incompatible type for column `a`: x numeric, y character\n- Incompatible type for column `b`: x numeric, y character",
    fixed = TRUE
  )
})

test_that("ignore column order", {
  expect_equal(
    all.equal(tibble(a = 1, b = 2), tibble(b = 2, a = 1), ignore_col_order = FALSE),
    "Same column names, but different order"
  )

  expect_equal(
    all.equal(tibble(a = 1, b = 2), tibble(a = 1), ignore_col_order = FALSE),
    "Cols in x but not y: `b`. "
  )
})
