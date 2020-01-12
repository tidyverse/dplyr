# Univariate keys --------------------------------------------------------------

a <- data.frame(x = c(1, 1, 2, 3), y = 1:4)
b <- data.frame(x = c(1, 2, 2, 4), z = 1:4)

test_that("univariate inner join has all columns, repeated matching rows", {
  j <- inner_join(a, b, "x")

  expect_equal(names(j), c("x", "y", "z"))
  expect_equal(j$y, c(1, 2, 3, 3))
  expect_equal(j$z, c(1, 1, 2, 3))
})

test_that("univariate left join has all columns, all rows", {
  j1 <- left_join(a, b, "x")
  j2 <- left_join(b, a, "x")

  expect_equal(names(j1), c("x", "y", "z"))
  expect_equal(names(j2), c("x", "z", "y"))

  expect_equal(j1$z, c(1, 1, 2, 3, NA))
  expect_equal(j2$y, c(1, 2, 3, 3, NA))
})

test_that("univariate semi join has x columns, matching rows", {
  j1 <- semi_join(a, b, "x")
  j2 <- semi_join(b, a, "x")

  expect_equal(names(j1), c("x", "y"))
  expect_equal(names(j2), c("x", "z"))

  expect_equal(j1$y, 1:3)
  expect_equal(j2$z, 1:3)
})

test_that("univariate anti join has x columns, missing rows", {
  j1 <- anti_join(a, b, "x")
  j2 <- anti_join(b, a, "x")

  expect_equal(names(j1), c("x", "y"))
  expect_equal(names(j2), c("x", "z"))

  expect_equal(j1$y, 4)
  expect_equal(j2$z, 4)
})

test_that("univariate right join has all columns, all rows", {
  j1 <- right_join(a, b, "x")
  j2 <- right_join(b, a, "x")

  expect_equal(names(j1), c("x", "y", "z"))
  expect_equal(names(j2), c("x", "z", "y"))

  expect_equal(j1$x, c(1, 1, 2, 2, 4))
  expect_equal(j1$y, c(1, 2, 3, 3, NA))
  expect_equal(j1$z, c(1, 1, 2, 3, 4))

  expect_equal(j2$x, c(1, 1, 2, 2, 3))
  expect_equal(j2$y, c(1, 2, 3, 3, 4))
  expect_equal(j2$z, c(1, 1, 2, 3, NA))
})

# Bivariate keys ---------------------------------------------------------------

c <- data.frame(
  x = c(1, 1, 2, 3),
  y = c(1, 1, 2, 3),
  a = 1:4
)
d <- data.frame(
  x = c(1, 2, 2, 4),
  y = c(1, 2, 2, 4),
  b = 1:4
)

test_that("bivariate inner join has all columns, repeated matching rows", {
  j <- inner_join(c, d, c("x", "y"))

  expect_equal(names(j), c("x", "y", "a", "b"))
  expect_equal(j$a, c(1, 2, 3, 3))
  expect_equal(j$b, c(1, 1, 2, 3))
})

test_that("bivariate left join has all columns, all rows", {
  j1 <- left_join(c, d, c("x", "y"))
  j2 <- left_join(d, c, c("x", "y"))

  expect_equal(names(j1), c("x", "y", "a", "b"))
  expect_equal(names(j2), c("x", "y", "b", "a"))

  expect_equal(j1$b, c(1, 1, 2, 3, NA))
  expect_equal(j2$a, c(1, 2, 3, 3, NA))
})

test_that("bivariate semi join has x columns, matching rows", {
  j1 <- semi_join(c, d, c("x", "y"))
  j2 <- semi_join(d, c, c("x", "y"))

  expect_equal(names(j1), c("x", "y", "a"))
  expect_equal(names(j2), c("x", "y", "b"))

  expect_equal(j1$a, 1:3)
  expect_equal(j2$b, 1:3)
})

test_that("bivariate anti join has x columns, missing rows", {
  j1 <- anti_join(c, d, c("x", "y"))
  j2 <- anti_join(d, c, c("x", "y"))

  expect_equal(names(j1), c("x", "y", "a"))
  expect_equal(names(j2), c("x", "y", "b"))

  expect_equal(j1$a, 4)
  expect_equal(j2$b, 4)
})


# Duplicate column names --------------------------------------------------

e <- data.frame(x = c(1, 1, 2, 3), z = 1:4)
f <- data.frame(x = c(1, 2, 2, 4), z = 1:4)

test_that("univariate inner join has all columns, repeated matching rows", {
  j <- inner_join(e, f, "x")

  expect_equal(names(j), c("x", "z.x", "z.y"))
  expect_equal(j$z.x, c(1, 2, 3, 3))
  expect_equal(j$z.y, c(1, 1, 2, 3))
})

test_that("univariate left join has all columns, all rows", {
  j1 <- left_join(e, f, "x")
  j2 <- left_join(f, e, "x")

  expect_equal(names(j1), c("x", "z.x", "z.y"))
  expect_equal(names(j2), c("x", "z.x", "z.y"))

  expect_equal(j1$z.y, c(1, 1, 2, 3, NA))
  expect_equal(j2$z.y, c(1, 2, 3, 3, NA))
})

# Misc --------------------------------------------------------------------

test_that("joins don't reorder columns #328", {
  a <- data.frame(a = 1:3)
  b <- data.frame(a = 1:3, b = 1, c = 2, d = 3, e = 4, f = 5)
  res <- left_join(a, b, "a")
  expect_equal(names(res), names(b))
})

test_that("indices don't get mixed up when nrow(x) > nrow(y). #365", {
  a <- data.frame(V1 = c(0, 1, 2), V2 = c("a", "b", "c"), stringsAsFactors = FALSE)
  b <- data.frame(V1 = c(0, 1), V3 = c("n", "m"), stringsAsFactors = FALSE)
  res <- inner_join(a, b, by = "V1")
  expect_equal(res$V1, c(0, 1))
  expect_equal(res$V2, c("a", "b"))
  expect_equal(res$V3, c("n", "m"))
})

test_that("inner_join is symmetric (even when joining on character & factor)", {
  foo <- tibble(id = factor(c("a", "b")), var1 = "foo")
  bar <- tibble(id = c("a", "b"), var2 = "bar")

  tmp1 <- inner_join(foo, bar, by = "id")
  tmp2 <- inner_join(bar, foo, by = "id")

  expect_is(tmp1$id, "character")
  expect_is(tmp2$id, "character")

  expect_equal(names(tmp1), c("id", "var1", "var2"))
  expect_equal(names(tmp2), c("id", "var2", "var1"))

  expect_equal(tmp1, tmp2[names(tmp1)])
})

test_that("left_join by different variable names (#617)", {
  x <- tibble(x1 = c(1, 3, 2))
  y <- tibble(y1 = c(1, 2, 3), y2 = c("foo", "foo", "bar"))
  res <- left_join(x, y, by = c("x1" = "y1"))
  expect_equal(names(res), c("x1", "y2"))
  expect_equal(res$x1, c(1, 3, 2))
  expect_equal(res$y2, c("foo", "bar", "foo"))
})

test_that("right_join gets the column in the right order #96", {
  a <- data.frame(x = 1:10, y = 2:11)
  b <- data.frame(x = 5:14, z = 3:12)
  res <- right_join(a, b)
  expect_equal(names(res), c("x", "y", "z"))

  a <- data.frame(x = 1:10, y = 2:11)
  b <- data.frame(z = 5:14, a = 3:12)
  res <- right_join(a, b, by = c("x" = "z"))
  expect_equal(names(res), c("x", "y", "a"))
})

test_that("full_join #96", {
  a <- data.frame(x = 1:3, y = 2:4)
  b <- data.frame(x = 3:5, z = 3:5)
  res <- full_join(a, b, "x")
  expect_equal(res$x, 1:5)
  expect_equal(res$y[1:3], 2:4)
  expect_true(all(is.na(res$y[4:5])))

  expect_true(all(is.na(res$z[1:2])))
  expect_equal(res$z[3:5], 3:5)
})

test_that("inner_join does not reorder (#684)", {
  test <- tibble(Greek = c("Alpha", "Beta", "Gamma"), Letters = LETTERS[1:3])
  lookup <- tibble(Letters = c("C", "B", "C"))
  res <- inner_join(lookup, test)
  expect_equal(res$Letters, c("C", "B", "C"))
})

test_that("group column names reflect renamed duplicate columns (#2330)", {
  d1 <- tibble(x = 1:5, y = 1:5) %>% group_by(x, y)
  d2 <- tibble(x = 1:5, y = 1:5)
  res <- inner_join(d1, d2, by = "x")
  expect_groups(d1, c("x", "y"))
  expect_groups(res, c("x", "y.x"))
})

test_that("group column names are null when joined data frames are not grouped (#2330)", {
  d1 <- tibble(x = 1:5, y = 1:5)
  d2 <- tibble(x = 1:5, y = 1:5)
  res <- inner_join(d1, d2, by = "x")
  expect_no_groups(res)
})

# Guessing variables in x and y ------------------------------------------------

test_that("join columns are not moved to the left (#802)", {
  df1 <- data.frame(x = 1, y = 1:5)
  df2 <- data.frame(y = 1:5, z = 2)

  out <- left_join(df1, df2)
  expect_equal(names(out), c("x", "y", "z"))
})

test_that("join creates correctly named results (#855)", {
  x <- tibble(q = c("a", "b", "c"), r = c("d", "e", "f"), s = c("1", "2", "3"))
  y <- tibble(q = c("a", "b", "c"), r = c("d", "e", "f"), t = c("xxx", "xxx", "xxx"))
  res <- left_join(x, y, by = c("r", "q"))
  expect_equal(names(res), c("q", "r", "s", "t"))
  expect_equal(res$q, x$q)
  expect_equal(res$r, x$r)
})

test_that("inner join gives same result as merge by default (#1281)", {
  set.seed(75)
  x <- data.frame(
    cat1 = sample(c("A", "B", NA), 5, 1),
    cat2 = sample(c(1, 2, NA), 5, 1), v = rpois(5, 3),
    stringsAsFactors = FALSE
  )
  y <- data.frame(
    cat1 = sample(c("A", "B", NA), 5, 1),
    cat2 = sample(c(1, 2, NA), 5, 1), v = rpois(5, 3),
    stringsAsFactors = FALSE
  )
  ij <- inner_join(x, y, by = c("cat1", "cat2"))
  me <- merge(x, y, by = c("cat1", "cat2"))
  expect_true(equal_data_frame(ij, me))
})

test_that("joins matches NA in character vector by default (#892, #2033)", {
  x <- data.frame(
    id = c(NA_character_, NA_character_),
    stringsAsFactors = F
  )

  y <- expand.grid(
    id = c(NA_character_, NA_character_),
    LETTER = LETTERS[1:2],
    stringsAsFactors = F
  )

  res <- left_join(x, y, by = "id")
  expect_true(all(is.na(res$id)))
  expect_equal(res$LETTER, rep(rep(c("A", "B"), each = 2), 2))
})

# Joined columns result in correct type ----------------------------------------

test_that("anti and semi joins give correct result when by variable is a factor (#1571)", {
  big <- data.frame(letter = rep(c("a", "b"), each = 2), number = 1:2)
  small <- data.frame(letter = "b")
  aj_result <- anti_join(big, small, by = "letter")
  expect_equal(aj_result$number, 1:2)
  expect_equal(aj_result$letter, factor(c("a", "a"), levels = c("a", "b")))

  sj_result <- semi_join(big, small, by = "letter")
  expect_equal(sj_result$number, 1:2)
  expect_equal(sj_result$letter, factor(c("b", "b"), levels = c("a", "b")))
})

# Encoding ----------------------------------------------------------------

test_that("left_join handles mix of encodings in column names (#1571)", {
  with_non_utf8_encoding({
    special <- get_native_lang_string()

    df1 <- tibble(x = 1:6, foo = 1:6)
    names(df1)[1] <- special

    df2 <- tibble(x = 1:6, baz = 1:6)
    names(df2)[1] <- enc2native(special)

    expect_message(res <- left_join(df1, df2), special, fixed = TRUE)
    expect_equal(names(res), c(special, "foo", "baz"))
    expect_equal(res$foo, 1:6)
    expect_equal(res$baz, 1:6)
    expect_equal(res[[special]], 1:6)
  })
})

# Misc --------------------------------------------------------------------

test_that("NAs match in joins only with na_matches = 'na' (#2033)", {
  skip("until https://github.com/r-lib/vctrs/issues/718")

  df1 <- tibble(a = NA)
  df2 <- tibble(a = NA, b = 1:3)
  for (na_matches in c("na", "never")) {
    accept_na_match <- (na_matches == "na")
    expect_equal(inner_join(df1, df2, na_matches = na_matches) %>% nrow(), 0 + 3 * accept_na_match)
    expect_equal(left_join(df1, df2, na_matches = na_matches) %>% nrow(), 1 + 2 * accept_na_match)
    expect_equal(right_join(df2, df1, na_matches = na_matches) %>% nrow(), 1 + 2 * accept_na_match)
    expect_equal(full_join(df1, df2, na_matches = na_matches) %>% nrow(), 4 - accept_na_match)
    expect_equal(anti_join(df1, df2, na_matches = na_matches) %>% nrow(), 1 - accept_na_match)
    expect_equal(semi_join(df1, df2, na_matches = na_matches) %>% nrow(), 0 + accept_na_match)
  }
})

test_that("joins regroups (#1597, #3566)", {
  df1 <- tibble(a = 1:3) %>% group_by(a)
  df2 <- tibble(a = rep(1:4, 2)) %>% group_by(a)

  expect_grouped <- function(df) {
    expect_true(is_grouped_df(df))
  }

  expect_grouped(inner_join(df1, df2))
  expect_grouped(left_join(df1, df2))
  expect_grouped(right_join(df2, df1))
  expect_grouped(full_join(df1, df2))
  expect_grouped(anti_join(df1, df2))
  expect_grouped(semi_join(df1, df2))
})

test_that("semi- and anti-joins preserve order (#2964)", {
  expect_identical(
    tibble(a = 3:1) %>% semi_join(tibble(a = 1:3)),
    tibble(a = 3:1)
  )
  expect_identical(
    tibble(a = 3:1) %>% anti_join(tibble(a = 4:6)),
    tibble(a = 3:1)
  )
})

test_that("nest_join works (#3570)",{
  df1 <- tibble(x = c(1, 2), y = c(2, 3))
  df2 <- tibble(x = c(1, 1), z = c(2, 3))
  res <- nest_join(df1, df2, by = "x")
  expect_equal(names(res), c(names(df1), "df2"))
  expect_identical(res$df2[[1]], select(df2, z))
  expect_identical(res$df2[[2]], tibble(z = double()))
})

test_that("nest_join handles multiple matches in x (#3642)", {
  df1 <- tibble(x = c(1, 1))
  df2 <- tibble(x = 1, y = 1:2)

  tbls <- df1 %>%
    nest_join(df2) %>%
    pull()

  expect_identical(tbls[[1]], tbls[[2]])
})

test_that("left_join() respects original row orders of x (#4639)", {
  d1 <- tibble(a = c(1:3, 3:1))
  d2 <- tibble(a = 3:1, b = 1:3)

  res <- left_join(d1, d2, by = "a")
  expect_equal(res$a, d1$a)
  expect_equal(res$b, c(3:1, 1:3))

  d1 <- tibble(a = c(1:3, 3:1))
  d2 <- tibble(a = c(3:1, 1), b = 1:4)

  res <- left_join(d1, d2, by = "a")
  expect_equal(res$a, c(1, 1:3, 3:1, 1))
  expect_equal(res$b, c(3:4, 2:1, 1:2, 3:4))
})

test_that("right_join() respects original row orders of x (#4639)", {
  d1 <- tibble(a = c(3:1))
  d2 <- tibble(a = c(1:3, 3:1), b = 1:6)

  res <- right_join(d1, d2)
  expect_equal(res$a, rep(d1$a, each = 2))
})

test_that("full_join() and right_join() correctly restores columns (#4649)", {
  df1 <- tibble(x = "x", by = 1)
  df2 <- tibble(y = "y", by = 1)
  res <- tibble(x = "x", by = 1, y = "y")

  expect_equal(res, full_join(df1, df2, by = "by"))
  expect_equal(res, right_join(df1, df2, by ="by"))
})

test_that("full_join() correctly binds the by part", {
  df1 <- tibble(x = "x", a = 1)
  df2 <- tibble(y = "y", b = 1)

  expect_equal(
    full_join(df1, df2, by = c("x" = "y")),
    tibble(x = c("x", "y"), a = c(1, NA), b = c(NA, 1))
  )
})
