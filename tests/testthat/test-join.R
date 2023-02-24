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

test_that("can't use `keep = FALSE` with non-equi conditions (#6499)", {
  df1 <- tibble(xl = c(1, 3), xu = c(4, 7))
  df2 <- tibble(yl = c(2, 5, 8), yu = c(6, 8, 9))

  expect_snapshot(error = TRUE, {
    left_join(df1, df2, join_by(overlaps(xl, xu, yl, yu)), keep = FALSE)
  })

  # Would never make sense here.
  # Based on how the binary conditions are generated we'd merge:
  # - `yu` into `xl`
  # - `yl` into `xu`
  # Which results in `xl` and `xu` columns that don't maintain `xl <= xu`.
  expect_snapshot(error = TRUE, {
    full_join(df1, df2, join_by(overlaps(xl, xu, yl, yu)), keep = FALSE)
  })
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

test_that("joins using `between(bounds =)` work as expected (#6488)", {
  df1 <- tibble(x = 1:5)
  df2 <- tibble(lower = 2, upper = 4)

  out <- full_join(df1, df2, by = join_by(between(x, lower, upper, bounds = "[]")))
  expect_identical(out$lower, c(NA, 2, 2, 2, NA))
  expect_identical(out$upper, c(NA, 4, 4, 4, NA))

  out <- full_join(df1, df2, by = join_by(between(x, lower, upper, bounds = "[)")))
  expect_identical(out$lower, c(NA, 2, 2, NA, NA))
  expect_identical(out$upper, c(NA, 4, 4, NA, NA))

  out <- full_join(df1, df2, by = join_by(between(x, lower, upper, bounds = "(]")))
  expect_identical(out$lower, c(NA, NA, 2, 2, NA))
  expect_identical(out$upper, c(NA, NA, 4, 4, NA))

  out <- full_join(df1, df2, by = join_by(between(x, lower, upper, bounds = "()")))
  expect_identical(out$lower, c(NA, NA, 2, NA, NA))
  expect_identical(out$upper, c(NA, NA, 4, NA, NA))
})

test_that("joins using `overlaps(bounds =)` work as expected (#6488)", {
  df1 <- tibble(x_lower = c(1, 1, 3, 4), x_upper = c(2, 3, 4, 5))
  df2 <- tibble(y_lower = 2, y_upper = 4)

  expect_closed <- vec_cbind(df1, vec_c(df2, df2, df2, df2))

  out <- full_join(df1, df2, by = join_by(overlaps(x_lower, x_upper, y_lower, y_upper, bounds = "[]")))
  expect_identical(out, expect_closed)

  # `[)`, `(]`, and `()` all generate the same binary conditions but are useful
  # for consistency with `between(bounds =)`
  expect_open <- vec_cbind(df1, vec_c(NA, df2, df2, NA))

  out <- full_join(df1, df2, by = join_by(overlaps(x_lower, x_upper, y_lower, y_upper, bounds = "[)")))
  expect_identical(out, expect_open)
  out <- full_join(df1, df2, by = join_by(overlaps(x_lower, x_upper, y_lower, y_upper, bounds = "(]")))
  expect_identical(out, expect_open)
  out <- full_join(df1, df2, by = join_by(overlaps(x_lower, x_upper, y_lower, y_upper, bounds = "()")))
  expect_identical(out, expect_open)
})

test_that("join_mutate() validates arguments", {
  df <- tibble(x = 1)

  # Mutating joins
  expect_snapshot(error = TRUE, {
    join_mutate(df, df, by = 1, type = "left")
    join_mutate(df, df, by = "x", type = "left", suffix = 1)
    join_mutate(df, df, by = "x", type = "left", na_matches = "foo")
    join_mutate(df, df, by = "x", type = "left", keep = 1)
  })
})

test_that("join_filter() validates arguments", {
  df <- tibble(x = 1)

  # Filtering joins
  expect_snapshot(error = TRUE, {
    join_filter(df, df, by = 1, type = "semi")
    join_filter(df, df, by = "x", type = "semi", na_matches = "foo")
  })
})

test_that("mutating joins trigger many-to-many warning", {
  df <- tibble(x = c(1, 1))
  expect_snapshot(out <- left_join(df, df, join_by(x)))
})

test_that("mutating joins don't trigger many-to-many warning when called indirectly", {
  df <- tibble(x = c(1, 1))

  fn <- function(df1, df2, relationship = NULL) {
    left_join(df1, df2, join_by(x), relationship = relationship)
  }

  # Directly calling `left_join()` from a function you control results in a warning
  expect_warning(fn(df, df), class = "dplyr_warning_join_relationship_many_to_many")

  # Now mimic calling an "rlang function" which you don't control that calls `left_join()`
  fn_env(fn) <- ns_env("rlang")

  # Indirectly calling `left_join()` through a function you don't control
  # doesn't warn
  expect_no_warning(fn(df, df), class = "dplyr_warning_join_relationship_many_to_many")
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

test_that("mutating joins reference original column in `y` when there are type errors (#6465)", {
  x <- tibble(a = 1)
  y <- tibble(b = "1")

  expect_snapshot({
    (expect_error(left_join(x, y, by = join_by(a == b))))
  })
})

test_that("filtering joins reference original column in `y` when there are type errors (#6465)", {
  x <- tibble(a = 1)
  y <- tibble(b = "1")

  expect_snapshot({
    (expect_error(semi_join(x, y, by = join_by(a == b))))
  })
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
  out <- nest_join(df1, df2, by = "x")

  expect_named(out, c("x", "y", "df2"))
  expect_type(out$df2, "list")
  expect_s3_class(out$df2[[1]], "tbl_df")
})

test_that("nest_join respects types of y (#6295)",{
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- rowwise(tibble(x = c(1, 1), z = c(2, 3)))
  out <- nest_join(df1, df2, by = "x")

  expect_s3_class(out$df2[[1]], "rowwise_df")
})

test_that("nest_join preserves data frame attributes on `x` and `y` (#6295)", {
  df1 <- data.frame(x = c(1, 2), y = c(3, 4))
  attr(df1, "foo") <- 1
  df2 <- data.frame(x = c(1, 2), z = c(3, 4))
  attr(df2, "foo") <- 2

  out <- nest_join(df1, df2, by = "x")
  expect_identical(attr(out, "foo"), 1)
  expect_identical(attr(out$df2[[1]], "foo"), 2)
})

test_that("nest_join computes common columns", {
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 3), z = c(2, 3))
  expect_snapshot(out <- nest_join(df1, df2))
})

test_that("nest_join references original column in `y` when there are type errors (#6465)", {
  x <- tibble(a = 1)
  y <- tibble(b = "1")

  expect_snapshot({
    (expect_error(nest_join(x, y, by = join_by(a == b))))
  })
})

test_that("nest_join handles multiple matches in x (#3642)", {
  df1 <- tibble(x = c(1, 1))
  df2 <- tibble(x = 1, y = 1:2)

  out <- nest_join(df1, df2, by = "x")
  expect_equal(out$df2[[1]], out$df2[[2]])
})

test_that("nest_join forces `multiple = 'all'` internally (#6392)", {
  df1 <- tibble(x = 1)
  df2 <- tibble(x = 1, y = 1:2)

  expect_no_warning(out <- nest_join(df1, df2, by = "x"))
  expect_identical(nrow(out$df2[[1]]), 2L)
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

test_that("validates inputs", {
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 3), z = c(2, 3))

  expect_snapshot(error = TRUE, {
    nest_join(df1, df2, by = 1)
    nest_join(df1, df2, keep = 1)
    nest_join(df1, df2, name = 1)
    nest_join(df1, df2, na_matches = 1)
  })

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

  # once for x + once for each row for y
  i <- count_regroups(out <- nest_join(gf1, gf2, by = "a"))
  expect_equal(i, 4L)
  expect_equal(group_vars(out), "a")
  expect_equal(group_vars(out$gf2[[1]]), "b")
})

test_that("joins respect zero length groups", {
  df1 <- tibble(f = factor( c(1,1,2,2), levels = 1:3), x = c(1,2,1,4)) %>%
    group_by(f)

  df2 <- tibble(f = factor( c(2,2,3,3), levels = 1:3), y = c(1,2,3,4)) %>%
    group_by(f)

  expect_equal(group_size(left_join( df1, df2, by = "f", relationship = "many-to-many")),  c(2,4))
  expect_equal(group_size(right_join( df1, df2, by = "f", relationship = "many-to-many")),  c(4,2))
  expect_equal(group_size(full_join( df1, df2, by = "f", relationship = "many-to-many")),  c(2,4,2))
  expect_equal(group_size(anti_join( df1, df2, by = "f")),  c(2))
  expect_equal(group_size(inner_join( df1, df2, by = "f", relationship = "many-to-many")),  c(4))


  df1 <- tibble(f = factor( c(1,1,2,2), levels = 1:3), x = c(1,2,1,4)) %>%
    group_by(f, .drop = FALSE)
  df2 <- tibble(f = factor( c(2,2,3,3), levels = 1:3), y = c(1,2,3,4)) %>%
    group_by(f, .drop = FALSE)

  expect_equal(group_size(left_join( df1, df2, by = "f", relationship = "many-to-many")),  c(2,4,0))
  expect_equal(group_size(right_join( df1, df2, by = "f", relationship = "many-to-many")),  c(0,4,2))
  expect_equal(group_size(full_join( df1, df2, by = "f", relationship = "many-to-many")),  c(2,4,2))
  expect_equal(group_size(anti_join( df1, df2, by = "f")),  c(2,0,0))
  expect_equal(group_size(inner_join( df1, df2, by = "f", relationship = "many-to-many")),  c(0,4,0))
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

  x <- left_join(df1, df2, by = "x")

  expect_identical(group_rows(x), list_of(1L, 2L, 3L))
})

# deprecated ----------------------------------------------------------------

test_that("by = character() generates cross (#4206)", {
  local_options(lifecycle_verbosity = "quiet")

  df1 <- tibble(x = 1:2)
  df2 <- tibble(y = 1:2)
  out <- left_join(df1, df2, by = character())

  expect_equal(out$x, rep(1:2, each = 2))
  expect_equal(out$y, rep(1:2, 2))
})

test_that("`by = character()` technically respects `unmatched`", {
  local_options(lifecycle_verbosity = "quiet")

  df1 <- tibble()
  df2 <- tibble(x = 1)

  expect_snapshot(error = TRUE, {
    left_join(df1, df2, by = character(), unmatched = "error")
  })
})

test_that("`by = character()` technically respects `relationship`", {
  local_options(lifecycle_verbosity = "quiet")

  df <- tibble(x = 1:2)

  expect_snapshot(error = TRUE, {
    left_join(df, df, by = character(), relationship = "many-to-one")
  })
})

test_that("`by = character()` for a cross join is deprecated (#6604)", {
  df1 <- tibble(x = 1:2)
  df2 <- tibble(y = 1:2)

  # Mutating join
  expect_snapshot({
    out <- left_join(df1, df2, by = character())
  })

  # Filtering join
  expect_snapshot({
    out <- semi_join(df1, df2, by = character())
  })

  # Nest join
  expect_snapshot({
    out <- nest_join(df1, df2, by = character())
  })
})

test_that("`by = named character()` for a cross join works", {
  # Used by the sift package
  df1 <- tibble(x = 1:2)
  df2 <- tibble(y = 1:2)

  by <- set_names(character(), nm = character())

  expect_snapshot({
    out <- left_join(df1, df2, by = by)
  })
  expect_identical(
    out,
    cross_join(df1, df2)
  )
})

test_that("`by = list(x = character(), y = character())` for a cross join is deprecated (#6604)", {
  df1 <- tibble(x = 1:2)
  df2 <- tibble(y = 1:2)

  expect_snapshot({
    out <- left_join(df1, df2, by = list(x = character(), y = character()))
  })
})
