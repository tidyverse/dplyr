context("Mutate")

test_that("empty mutate returns input", {
  df <- tibble(x = 1)
  gf <- group_by(df, x)

  expect_equal(mutate(df), df)
  expect_equal(mutate(gf), gf)
})

test_that("mutations applied progressively", {
  df <- tibble(x = 1)
  expect_equal(df %>% mutate(y = x + 1, z = y + 1), tibble(x = 1, y = 2, z = 3))
  expect_equal(df %>% mutate(x = x + 1, x = x + 1), tibble(x = 3))
})

test_that("length-1 vectors are recycled (#152)", {
  df <- tibble(x = 1:4)
  expect_equal(mutate(df, y = 1)$y, rep(1, 4))
  expect_error(mutate(df, y = 1:2), "must be recyclable")
})

test_that("can remove variables with NULL (#462)", {
  df <- tibble(x = 1:3, y = 1:3)
  gf <- group_by(df, x)

  expect_equal(df %>% mutate(y = NULL), df[1])
  expect_equal(gf %>% mutate(y = NULL), gf[1])

  # even if it doesn't exist
  expect_equal(df %>% mutate(z = NULL), df)
  # or was just created
  expect_equal(df %>% mutate(z = 1, z = NULL), df)
})

test_that("mutate() names pronouns correctly (#2686)", {
  expect_named(mutate(tibble(x = 1), .data$x), "x")
  expect_named(mutate(tibble(x = 1), .data[["x"]]), "x")
})

test_that("mutate() supports unquoted values", {
  df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)
  expect_identical(mutate(df, out = !!1), mutate(df, out = 1))
  expect_identical(mutate(df, out = !!(1:5)), mutate(df, out = 1:5))
  expect_identical(mutate(df, out = !!quote(1:5)), mutate(df, out = 1:5))

  gdf <- group_by(df, g)
  expect_identical(mutate(gdf, out = !!1), mutate(gdf, out = 1))
})

test_that("assignments don't overwrite variables (#315)", {
  df <- tibble(x = 1, y = 2)
  out <- df %>% mutate(z = {x <- 10; x})
  expect_equal(out, tibble(x = 1, y = 2, z = 10))
})

# column types ------------------------------------------------------------

test_that("mutate disambiguates NA and NaN (#1448)", {
  df <- tibble(x = c(1, NA, NaN))
  out <- mutate(df, y = x * 1)
  expect_equal(out$y, df$x)
})

test_that("mutate preserves names (#1689, #2675)", {
  df <- tibble(a = 1:3)
  out1 <- df %>% mutate(b = setNames(1:3, letters[1:3]))
  out2 <- df %>% mutate(b = setNames(as.list(1:3), letters[1:3]))

  expect_named(out1$b, letters[1:3])
  expect_named(out2$b, letters[1:3])
})

test_that("mutate handles data frame columns", {
  df <- data.frame("a" = c(1, 2, 3), "b" = c(2, 3, 4), "base_col" = c(3, 4, 5))
  res <- mutate(df, new_col = data.frame(x = 1:3))
  expect_equal(res$new_col, data.frame(x = 1:3))

  res <- mutate(group_by(df, a), new_col = data.frame(x = a))
  expect_equal(res$new_col, data.frame(x = 1:3))

  res <- mutate(rowwise(df), new_col = data.frame(x = a))
  expect_equal(res$new_col, data.frame(x = 1:3))
})

test_that("unnamed data frames are automatically unspliced  (#2326, #3630)", {
  expect_identical(
    tibble(a = 1) %>% mutate(tibble(b = 2)),
    tibble(a = 1, b = 2)
  )
  expect_identical(
    tibble(a = 1) %>% mutate(tibble(b = 2), tibble(b = 3)),
    tibble(a = 1, b = 3)
  )
  expect_identical(
    tibble(a = 1) %>% mutate(tibble(b = 2), c = b),
    tibble(a = 1, b = 2, c = 2)
  )
})

test_that("named data frames are packed (#2326, #3630)", {
  df <- tibble(x = 1)
  out <- df %>% mutate(y = tibble(a = x))
  expect_equal(out, tibble(x = 1, y = tibble(a = 1)))
})

# output types ------------------------------------------------------------

test_that("mutate preserves grouping", {
  gf <- group_by(tibble(x = 1:2, y = 2), x)

  i <- count_regroups(out <- mutate(gf, x = 1))
  expect_equal(i, 1L)
  expect_equal(group_vars(out), "x")
  expect_equal(nrow(group_data(out)), 1)

  i <- count_regroups(out <- mutate(gf, z = 1))
  expect_equal(i, 0)
  expect_equal(group_data(out), group_data(gf))
})

test_that("mutate works on zero-row grouped data frame (#596)", {
  dat <- data.frame(a = numeric(0), b = character(0))
  res <- dat %>% group_by(b, .drop = FALSE) %>% mutate(a2 = a * 2)
  expect_is(res$a2, "numeric")
  expect_is(res, "grouped_df")
  expect_equal(res$a2, numeric(0))

  expect_type(group_rows(res), "list")
  expect_equal(attr(group_rows(res), "ptype"), integer())
  expect_equal(group_data(res)$b, factor(character(0)))
})

test_that("mutate works on zero-row rowwise data frame (#4224)", {
  dat <- data.frame(a = numeric(0))
  res <- dat %>% rowwise() %>% mutate(a2 = a * 2)
  expect_is(res$a2, "numeric")
  expect_is(res, "rowwise_df")
  expect_equal(res$a2, numeric(0))
})


test_that("mutate works on empty data frames (#1142)", {
  df <- data.frame()
  res <- df %>% mutate()
  expect_equal(nrow(res), 0L)
  expect_equal(length(res), 0L)

  res <- df %>% mutate(x = numeric())
  expect_equal(names(res), "x")
  expect_equal(nrow(res), 0L)
  expect_equal(length(res), 1L)
})

test_that("mutate handles 0 rows rowwise (#1300)", {
  res <- tibble(y = character()) %>% rowwise() %>% mutate(z = 1)
  expect_equal(nrow(res), 0L)
})

test_that("rowwise mutate gives expected results (#1381)", {
  f <- function(x) ifelse(x < 2, NA_real_, x)
  res <- tibble(x = 1:3) %>% rowwise() %>% mutate(y = f(x))
  expect_equal(res$y, c(NA, 2, 3))
})

test_that("grouped mutate does not drop grouping attributes (#1020)", {
  d <- data.frame(subject = c("Jack", "Jill"), id = c(2, 1)) %>% group_by(subject)
  a1 <- names(attributes(d))
  a2 <- names(attributes(d %>% mutate(foo = 1)))
  expect_equal(setdiff(a1, a2), character(0))
})

test_that("mutate() hands list columns with rowwise magic to follow up expressions (#4845)", {
  test <- rowwise(tibble(x = 1:2))

  expect_identical(
    test %>%
      mutate(a = list(1)) %>%
      mutate(b = list(a + 1)),
    test %>%
      mutate(a = list(1), b = list(a + 1))
  )
})


# other -------------------------------------------------------------------

test_that("no utf8 invasion (#722)", {
  skip_if_not(l10n_info()$"UTF-8")
  source("utf-8.txt", local = TRUE, encoding = "UTF-8")
})

test_that("mutate() to UTF-8 column names", {
  df <- tibble(a = 1) %>% mutate("\u5e78" := a)

  expect_equal(colnames(df), c("a", "\u5e78"))
})

test_that("Non-ascii column names in version 0.3 are not duplicated (#636)", {
  with_non_utf8_encoding({
    df <- tibble(a = "1", b = "2")
    names(df) <- c("a", enc2native("\u4e2d"))

    res <- df %>% mutate_all(as.numeric)
    expect_equal(names(res), names(df))
  })
})

test_that("mutate coerces results from one group with all NA values (#1463) ", {
  df <- tibble(x = c(1, 2), y = c(1, NA))
  res <- df %>% group_by(x) %>% mutate(z = ifelse(y > 1, 1, 2))
  expect_true(is.na(res$z[2]))
  expect_is(res$z, "numeric")
})

test_that("grouped subsets are not lazy (#3360)", {
  make_call <- function(x) {
    quo(!!x)
  }

  res <- tibble(name = 1:2, value = letters[1:2]) %>%
    rowwise() %>%
    mutate(call = list(make_call(value))) %>%
    pull()

  expect_identical(res, list(make_call("a"), make_call("b")))

  res <- tibble(name = 1:2, value = letters[1:2]) %>%
    group_by(name) %>%
    mutate(call = list(make_call(value))) %>%
    pull()

  expect_identical(res, list(make_call("a"), make_call("b")))
})

test_that("mutate() evaluates expression for empty groups", {
  df <- tibble(f = factor(c("a", "b"), levels = c("a", "b", "c")))
  gf <- group_by(df, f, .drop = FALSE)

  count <- 0
  mutate(gf, x = {count <<- count + 1})
  expect_equal(count, 3L)
})

test_that("DataMask$add() forces chunks (#4677)", {
  df <- tibble(bf10 = 0.244) %>%
    mutate(
      bf01 = 1 / bf10,
      log_e_bf10 = log(bf10),
      log_e_bf01 = log(bf01)
    )
  expect_equal(df$log_e_bf01, log(1 / 0.244))
})


# .before, .after, .keep ------------------------------------------------------

test_that(".keep = 'unused' keeps variables explicitly mentioned", {
  df <- tibble(x = 1, y = 2)
  out <- mutate(df, x1 = x + 1, y = y, .keep = "unused")
  expect_named(out, c("y", "x1"))
})

test_that(".keep = 'used' not affected by across()", {
  df <- tibble(x = 1, y = 2, z = 3, a = "a", b = "b", c = "c")

  # This must evaluate every column in order to figure out if should
  # be included in the set or not, but that shouldn't be counted for
  # the purposes of "used" variables
  out <- mutate(df, across(is.numeric, identity), .keep = "unused")
  expect_named(out, names(df))
})

test_that(".keep = 'used' keeps variables used in expressions", {
  df <- tibble(a = 1, b = 2, c = 3, x = 1, y = 2)
  out <- mutate(df, xy = x + y, .keep = "used")
  expect_named(out, c("x", "y", "xy"))
})

test_that(".keep = 'none' only keeps grouping variables", {
  df <- tibble(x = 1, y = 2)
  gf <- group_by(df, x)

  expect_named(mutate(df, z = 1, .keep = "none"), "z")
  expect_named(mutate(gf, z = 1, .keep = "none"), c("x", "z"))
})

test_that(".keep = 'none' prefers new order", {
  df <- tibble(x = 1, y = 2)
  expect_named(df %>% mutate(y = 1, x = 2, .keep = "none"), c("y", "x"))

  # even when grouped
  gf <- group_by(df, x)
  expect_named(gf %>% mutate(y = 1, x = 2, .keep = "none"), c("y", "x"))
})

test_that("can use .before and .after to control column position", {
  df <- tibble(x = 1, y = 2)
  expect_named(mutate(df, z = 1), c("x", "y", "z"))
  expect_named(mutate(df, z = 1, .before = 1), c("z", "x", "y"))
  expect_named(mutate(df, z = 1, .after = 1), c("x", "z", "y"))

  # but doesn't affect order of existing columns
  df <- tibble(x = 1, y = 2)
  expect_named(mutate(df, x = 1, .after = y), c("x", "y"))
})

# Error messages ----------------------------------------------------------

test_that("mutate() give meaningful errors", {
  verify_output(test_path("test-mutate-errors.txt"), {
    tbl <- tibble(x = 1:2, y = 1:2)

    "# setting column to NULL makes it unavailable"
    tbl %>%
      mutate(y = NULL, a = sum(y))
    tbl %>%
      group_by(x) %>%
      mutate(y = NULL, a = sum(y))

    "# incompatible column type"
    tibble(x = 1) %>%
      mutate(y = mean)

    "# Unsupported type"
    df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)
    df %>%
        mutate(out = env(a = 1))
    df %>%
      group_by(g) %>%
      mutate(out = env(a = 1))
    df %>%
      rowwise() %>%
      mutate(out = rnorm)

    "# incompatible types across groups"
    data.frame(x = rep(1:5, each = 3)) %>%
      group_by(x) %>%
      mutate(val = ifelse(x < 3, "foo", 2))

    tibble(a = 1:3, b=4:6) %>%
      group_by(a) %>%
      mutate(if(a==1) NULL else "foo")

    "# incompatible size"
    data.frame(x = c(2, 2, 3, 3)) %>%
      mutate(int = 1:5)
    data.frame(x = c(2, 2, 3, 3)) %>%
      group_by(x) %>%
      mutate(int = 1:5)
    data.frame(x = c(2, 3, 3)) %>%
      group_by(x) %>%
      mutate(int = 1:5)
    data.frame(x = c(2, 2, 3, 3)) %>%
      rowwise() %>%
      mutate(int = 1:5)

    "# .data pronoun"
    tibble(a = 1) %>%
      mutate(c = .data$b)
    tibble(a = 1:3) %>%
      group_by(a) %>%
      mutate(c = .data$b)
  })
})
