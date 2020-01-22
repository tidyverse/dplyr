test_that("empty arrange() returns input", {
  df <- tibble(x = 1:10, y = 1:10)
  gf <- group_by(df, x)

  expect_reference(arrange(df), df)
  expect_reference(arrange(gf), gf)
})

test_that("can sort empty data frame", {
  df <- tibble(a = numeric(0))
  expect_equal(arrange(df, a), df)
})

test_that("local arrange sorts missing values to end", {
  df <- data.frame(x = c(2, 1, NA))

  expect_equal(df %>% arrange(x) %>% pull(), c(1, 2, NA))
  expect_equal(df %>% arrange(desc(x)) %>% pull(), c(2, 1, NA))
})

test_that("arrange() gives meaningful errors", {
  verify_output(test_path("test-arrange-errors.txt"), {
    "# duplicated column name"
    df <- data.frame(x = 1:10, x = 1:10)
    names(df) <- c("x", "x")
    df %>% arrange(x)

    df <- data.frame(x = 1:10, x = 1:10, y = 1:10, y = 1:10)
    names(df) <- c("x", "x", "y", "y")
    df %>% arrange(x)
  })
})

# column types ----------------------------------------------------------

test_that("arrange handles list columns (#282)", {
  # no intrinsic ordering
  df <- tibble(x = 1:3, y = list(3, 2, 1))
  expect_equal(arrange(df, y), df)

  df <- tibble(x = 1:3, y = list(sum, mean, sd))
  expect_equal(arrange(df, y), df)
})

test_that("arrange handles raw columns (#1803)", {
  df <- tibble(x = 1:3, y = as.raw(3:1))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange handles matrix columns", {
  df <- tibble(x = 1:3, y = matrix(6:1, ncol = 2))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange handles data.frame columns (#3153)", {
  df <- tibble(x = 1:3, y = data.frame(z = 3:1))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange handles complex columns", {
  df <- tibble(x = 1:3, y = 3:1 + 2i)
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange supports bit64::integer64 (#4366)", {
  df <- tibble(x = 1:3, y = bit64::as.integer64(3:1))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange handles S4 classes #1105", {
  skip("TODO: https://github.com/r-lib/vctrs/issues/776")
  TestS4 <- suppressWarnings(setClass("TestS4", contains = "integer"))
  setMethod('[', 'TestS4', function(x, i, ...){ TestS4(unclass(x)[i, ...])  })
  on.exit(removeClass("TestS4"))

  df <- tibble(x = 1:3, y = TestS4(3:1))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange respects locale (#1280)", {
  df2 <- tibble(words = c("casa", "\u00e1rbol", "zona", "\u00f3rgano"))

  res <- df2 %>% arrange(words)
  expect_equal(res$words, sort(df2$words))

  res <- df2 %>% arrange(desc(words))
  expect_equal(res$words, sort(df2$words, decreasing = TRUE))
})

# data ----------------------------------------------------------------

test_that("arrange preserves input class", {
  df1 <- data.frame(x = 1:3, y = 3:1)
  df2 <- tibble(x = 1:3, y = 3:1)
  df3 <- df1 %>% group_by(x)

  expect_s3_class(arrange(df1, x), "data.frame", exact = TRUE)
  expect_s3_class(arrange(df2, x), "tbl_df")
  expect_s3_class(arrange(df3, x), "grouped_df")
})

test_that("grouped arrange ignores group, unless requested with .by_group", {
  df <- data.frame(g = c(2, 1, 2, 1), x = 4:1)
  gf <- group_by(df, g)

  expect_equal(arrange(gf, x), gf[4:1, ,])
  expect_equal(arrange(gf, x, .by_group = TRUE), gf[c(4, 2, 3, 1), ,])
})

test_that("arrange updates the grouping structure (#605)", {
  df <- tibble(g = c(2, 2, 1, 1), x = c(1, 3, 2, 4))
  res <- df %>% group_by(g) %>% arrange(x)
  expect_s3_class(res, "grouped_df")
  expect_equal(group_rows(res), list_of(c(2L, 4L), c(1L, 3L)))
})

test_that("arrange() supports across() (#4679)", {
  df <- tibble(x = c(1, 3, 2, 1), y = c(4, 3, 2, 1))
  expect_identical(
    df %>% arrange(across()),
    df %>% arrange(x, y)
  )
  expect_identical(
    df %>% arrange(across(fns = desc)),
    df %>% arrange(desc(x), desc(y))
  )
  expect_identical(
    df %>% arrange(across(x)),
    df %>% arrange(x)
  )
  expect_identical(
    df %>% arrange(across(y)),
    df %>% arrange(y)
  )
})
