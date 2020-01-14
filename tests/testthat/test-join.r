# Basic properties --------------------------------------------------------

test_that("mutating joins preserve row and column order of x", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 1, c = 2, a = 4:1)

  out <- inner_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:3)

  out <- left_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:3)

  out <- right_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:4)

  out <- full_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:4)
})

test_that("even when column names change", {
  df1 <- data.frame(x = c(1, 1, 2, 3), z = 1:4, a = 1)
  df2 <- data.frame(z = 1:4, b = 1, x = c(1, 2, 2, 4))

  out <- inner_join(df1, df2, by = "x")
  expect_named(out, c("x", "z.x", "a", "z.y", "b"))
})

test_that("by = character() generates cross (#4206)", {
  df1 <- tibble(x = 1:2)
  df2 <- tibble(y = 1:2)
  out <- left_join(df1, df2, by = character())

  expect_equal(out$x, rep(1:2, each = 2))
  expect_equal(out$y, rep(1:2, 2))
})

test_that("filtering joins preserve row and column order of x (#2964)", {
  df1 <- data.frame(a = 4:1, b = 1)
  df2 <- data.frame(b = 1, c = 2, a = 2:3)

  out <- semi_join(df1, df2, by = "a")
  expect_named(out, c("a", "b"))
  expect_equal(out$a, 3:2)

  out <- anti_join(df1, df2, by = "a")
  expect_named(out, c("a", "b"))
  expect_equal(out$a, c(4L, 1L))
})

test_that("keys are coerced to symmetric type", {
  foo <- tibble(id = factor(c("a", "b")), var1 = "foo")
  bar <- tibble(id = c("a", "b"), var2 = "bar")

  expect_type(inner_join(foo, bar, by = "id")$id, "character")
  expect_type(inner_join(bar, foo, by = "id")$id, "character")
})

test_that("when keep = TRUE, full_join() preserves both sets of keys", {
  df1 <- tibble(a = c(2, 3), b = c(1, 2))
  df2 <- tibble(x = c(3, 4), y = c(3, 4))
  out <- full_join(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(2, 3, NA))
  expect_equal(out$x, c(NA, 3, 4))
})

test_that("joins matches NAs by default (#892, #2033)", {
  df1 <- tibble(x = c(NA_character_, 1))
  df2 <- tibble(x = c(NA_character_, 2))

  expect_equal(nrow(inner_join(df1, df2, by = "x")), 1)
  expect_equal(nrow(semi_join(df1, df2, by = "x")), 1)
})

test_that("joins don't match NA when na_matches = 'never' (#2033)", {
  df1 <- tibble(a = NA)
  df2 <- tibble(a = NA, b = 1:3)
  expect_warning(left_join(df1, df2, by = "a", na_matches = "never"))

  skip("until https://github.com/r-lib/vctrs/issues/718")
})

# nest_join ---------------------------------------------------------------

test_that("nest_join returns list of tibbles (#3570)",{
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 1), z = c(2, 3))
  out <- nest_join(df1, df2, by = "x")

  expect_type(out$df2, "list")
  expect_s3_class(out$df2[[1]], "tbl_df")
})

test_that("nest_join handles multiple matches in x (#3642)", {
  df1 <- tibble(x = c(1, 1))
  df2 <- tibble(x = 1, y = 1:2)

  out <- nest_join(df1, df2, by = "x")
  expect_equal(out$df2[[1]], out$df2[[2]])
})

test_that("y keys dropped by default", {
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 1), z = c(2, 3))
  out <- nest_join(df1, df2, by = "x")
  expect_named(out$df2[[1]], "z")

  out <- nest_join(df1, df2, by = "x", keep = TRUE)
  expect_named(out$df2[[1]], c("x", "z"))
})

# output type ---------------------------------------------------------------

test_that("joins x preserve type of x", {
  df1 <- data.frame(x = 1)
  df2 <- tibble(x = 2)

  expect_s3_class(inner_join(df1, df2, by = "x"), "data.frame", exact = TRUE)
  expect_s3_class(inner_join(df2, df1, by = "x"), "tbl_df")
})

test_that("joins preserve groups", {
  gf1 <- tibble(a = 1:3) %>% group_by(a)
  gf2 <- tibble(a = rep(1:4, 2), b = 1) %>% group_by(b)

  i <- count_regroups(out <- inner_join(gf1, gf2, by = "a"))
  expect_equal(i, 1L)
  expect_equal(group_vars(out), "a")

  i <- count_regroups(out <- semi_join(gf1, gf2, by = "a"))
  expect_equal(i, 0L)
  expect_equal(group_vars(out), "a")
})

test_that("group column names reflect renamed duplicate columns (#2330)", {
  df1 <- tibble(x = 1:5, y = 1:5) %>% group_by(x, y)
  df2 <- tibble(x = 1:5, y = 1:5)

  out <- inner_join(df1, df2, by = "x")
  expect_equal(group_vars(out), c("x", "y.x"))
})

