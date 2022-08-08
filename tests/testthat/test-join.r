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
  df2 <- data.frame(z = 1:3, b = 1, x = c(1, 2, 4))

  out <- inner_join(df1, df2, by = "x")
  expect_named(out, c("x", "z.x", "a", "z.y", "b"))
})

test_that("by = character() / by = join_by() generates cross (#4206)", {
  df1 <- tibble(x = 1:2)
  df2 <- tibble(y = 1:2)
  out <- left_join(df1, df2, by = character())

  expect_equal(out$x, rep(1:2, each = 2))
  expect_equal(out$y, rep(1:2, 2))

  out2 <- left_join(df1, df2, by = join_by())
  expect_identical(out, out2)
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

  df1 <- tibble(x = 1, y = factor("a"))
  df2 <- tibble(x = 2, y = factor("b"))
  out <- full_join(df1, df2, by = c("x", "y"))
  expect_equal(out$y, factor(c("a", "b")))
})

test_that("keys of non-equi conditions are not coerced if `keep = NULL`", {
  foo <- tibble(id = factor(c("a", "b")), col1 = c(1, 2), var1 = "foo")
  bar <- tibble(id = c("a", "b"), col2 = c(1L, 2L), var2 = "bar")

  out <- inner_join(foo, bar, by = join_by(id, col1 >= col2))
  expect_type(out$id, "character")
  expect_type(out$col1, "double")
  expect_type(out$col2, "integer")

  out <- inner_join(bar, foo, by = join_by(id, col2 <= col1))
  expect_type(out$id, "character")
  expect_type(out$col1, "double")
  expect_type(out$col2, "integer")

  # But they are if `keep = FALSE`
  out <- inner_join(bar, foo, by = join_by(id, col2 <= col1), keep = FALSE)
  expect_type(out$col2, "double")
})

test_that("when keep = TRUE, left_join() preserves both sets of keys", {
  # when keys have different names
  df1 <- tibble(a = c(2, 3), b = c(1, 2))
  df2 <- tibble(x = c(3, 4), y = c(3, 4))
  out <- left_join(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(2, 3))
  expect_equal(out$x, c(NA, 3))

  # when keys have same name
  df1 <- tibble(a = c(2, 3), b = c(1, 2))
  df2 <- tibble(a = c(3, 4), y = c(3, 4))
  out <- left_join(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(2, 3))
  expect_equal(out$a.y, c(NA, 3))
})

test_that("when keep = TRUE, right_join() preserves both sets of keys", {
  # when keys have different names
  df1 <- tibble(a = c(2, 3), b = c(1, 2))
  df2 <- tibble(x = c(3, 4), y = c(3, 4))
  out <- right_join(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(3, NA))
  expect_equal(out$x, c(3, 4))

  # when keys have same name
  df1 <- tibble(a = c(2, 3), b = c(1, 2))
  df2 <- tibble(a = c(3, 4), y = c(3, 4))
  out <- right_join(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(3, NA))
  expect_equal(out$a.y, c(3, 4))
})

test_that("when keep = TRUE, full_join() preserves both sets of keys", {
  # when keys have different names
  df1 <- tibble(a = c(2, 3), b = c(1, 2))
  df2 <- tibble(x = c(3, 4), y = c(3, 4))
  out <- full_join(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(2, 3, NA))
  expect_equal(out$x, c(NA, 3, 4))

  # when keys have same name
  df1 <- tibble(a = c(2, 3), b = c(1, 2))
  df2 <- tibble(a = c(3, 4), y = c(3, 4))
  out <- full_join(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(2, 3, NA))
  expect_equal(out$a.y, c(NA, 3, 4))
})

test_that("when keep = TRUE, inner_join() preserves both sets of keys (#5581)", {
  # when keys have different names
  df1 <- tibble(a = c(2, 3), b = c(1, 2))
  df2 <- tibble(x = c(3, 4), y = c(3, 4))
  out <- inner_join(df1, df2, by = c("a" = "x"), keep = TRUE)
  expect_equal(out$a, c(3))
  expect_equal(out$x, c(3))

  # when keys have same name
  df1 <- tibble(a = c(2, 3), b = c(1, 2))
  df2 <- tibble(a = c(3, 4), y = c(3, 4))
  out <- inner_join(df1, df2, by = c("a"), keep = TRUE)
  expect_equal(out$a.x, c(3))
  expect_equal(out$a.y, c(3))
})

test_that("joins matches NAs by default (#892, #2033)", {
  df1 <- tibble(x = c(NA_character_, 1))
  df2 <- tibble(x = c(NA_character_, 2))

  expect_equal(nrow(inner_join(df1, df2, by = "x")), 1)
  expect_equal(nrow(semi_join(df1, df2, by = "x")), 1)
})

test_that("joins don't match NA when na_matches = 'never' (#2033)", {
  df1 <- tibble(a = c(1, NA))
  df2 <- tibble(a = c(1, NA), b = 1:2)

  out <- left_join(df1, df2, by = "a", na_matches = "never")
  expect_equal(out, tibble(a = c(1, NA), b = c(1, NA)))

  out <- inner_join(df1, df2, by = "a", na_matches = "never")
  expect_equal(out, tibble(a = 1, b = 1))

  out <- semi_join(df1, df2, by = "a", na_matches = "never")
  expect_equal(out, tibble(a = 1))

  out <- anti_join(df1, df2, by = "a", na_matches = "never")
  expect_equal(out, tibble(a = NA_integer_))

  out <- nest_join(df1, df2, by = "a", na_matches = "never")
  expect <- tibble(a = c(1, NA), df2 = list(tibble(b = 1L), tibble(b = integer())))
  expect_equal(out, expect)

  dat1 <- tibble(
    name = c("a", "c"),
    var1 = c(1, 2)
  )
  dat3 <- tibble(
    name = c("a", NA_character_),
    var3 = c(5, 6)
  )
  expect_equal(
    full_join(dat1, dat3, by = "name", na_matches = "never"),
    tibble(name = c("a", "c", NA), var1 = c(1, 2, NA), var3 = c(5, NA, 6))
  )
})

test_that("`na_matches` is validated", {
  df <- tibble(x = 1)

  # Mutating joins
  expect_snapshot(error = TRUE, {
    join_mutate(df, df, by = "x", type = "left", na_matches = 1)
  })
  expect_snapshot(error = TRUE, {
    join_mutate(df, df, by = "x", type = "left", na_matches = "foo")
  })

  # Filtering joins
  expect_snapshot(error = TRUE, {
    join_filter(df, df, by = "x", type = "semi", na_matches = 1)
  })
  expect_snapshot(error = TRUE, {
    join_filter(df, df, by = "x", type = "semi", na_matches = "foo")
  })
})

test_that("`unmatched` is validated", {
  df <- tibble(x = 1)

  expect_snapshot(error = TRUE, {
    join_mutate(df, df, by = "x", type = "left", unmatched = 1)
  })
  expect_snapshot(error = TRUE, {
    join_mutate(df, df, by = "x", type = "left", unmatched = "foo")
  })
})

test_that("mutating joins compute common columns", {
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 3), z = c(2, 3))
  expect_snapshot(out <- left_join(df1, df2))
})

test_that("filtering joins compute common columns", {
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 3), z = c(2, 3))
  expect_snapshot(out <- semi_join(df1, df2))
})

test_that("error if passed additional arguments", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(a = 1)

  expect_snapshot(error = TRUE, {
    inner_join(df1, df2, on = "a")
    left_join(df1, df2, on = "a")
    right_join(df1, df2, on = "a")
    full_join(df1, df2, on = "a")
    nest_join(df1, df2, on = "a")
    anti_join(df1, df2, on = "a")
    semi_join(df1, df2, on = "a")
  })
})

# nest_join ---------------------------------------------------------------

test_that("nest_join returns list of tibbles (#3570)",{
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 1), z = c(2, 3))
  out <- nest_join(df1, df2, by = "x", multiple = "all")

  expect_named(out, c("x", "y", "df2"))
  expect_type(out$df2, "list")
  expect_s3_class(out$df2[[1]], "tbl_df")
})

test_that("nest_join computes common columns", {
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 3), z = c(2, 3))
  expect_snapshot(out <- nest_join(df1, df2))
})

test_that("nest_join handles multiple matches in x (#3642)", {
  df1 <- tibble(x = c(1, 1))
  df2 <- tibble(x = 1, y = 1:2)

  out <- nest_join(df1, df2, by = "x", multiple = "all")
  expect_equal(out$df2[[1]], out$df2[[2]])
})

test_that("y keys dropped by default for equi conditions", {
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 3), z = c(2, 3))
  out <- nest_join(df1, df2, by = "x")
  expect_named(out, c("x", "y", "df2"))
  expect_named(out$df2[[1]], "z")

  out <- nest_join(df1, df2, by = "x", keep = TRUE)
  expect_named(out$df2[[1]], c("x", "z"))
})

test_that("y keys kept by default for non-equi conditions", {
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 3), z = c(2, 3))

  out <- nest_join(df1, df2, by = join_by(x >= x))
  expect_named(out, c("x", "y", "df2"))
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

  i <- count_regroups(out <- inner_join(gf1, gf2, by = "a", multiple = "all"))
  expect_equal(i, 1L)
  expect_equal(group_vars(out), "a")

  i <- count_regroups(out <- semi_join(gf1, gf2, by = "a"))
  expect_equal(i, 0L)
  expect_equal(group_vars(out), "a")

  # See comment in nest_join
  i <- count_regroups(out <- nest_join(gf1, gf2, by = "a", multiple = "all"))
  expect_equal(i, 1L)
  expect_equal(group_vars(out), "a")
})

test_that("group column names reflect renamed duplicate columns (#2330)", {
  df1 <- tibble(x = 1:5, y = 1:5) %>% group_by(x, y)
  df2 <- tibble(x = 1:5, y = 1:5)

  out <- inner_join(df1, df2, by = "x")
  expect_equal(group_vars(out), "x")
  # TODO: fix this issue: https://github.com/tidyverse/dplyr/issues/4917
  # expect_equal(group_vars(out), c("x", "y.x"))
})

test_that("rowwise group structure is updated after a join (#5227)", {
  df1 <- rowwise(tibble(x = 1:2))
  df2 <- tibble(x = c(1:2, 2L))

  x <- left_join(df1, df2, by = "x", multiple = "all")

  expect_identical(group_rows(x), list_of(1L, 2L, 3L))
})
