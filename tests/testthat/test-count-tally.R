# count -------------------------------------------------------------------

test_that("count sorts output by keys by default", {
  # Due to usage of `summarise()` internally
  df <- tibble(x = c(2, 1, 1, 2, 1))
  out <- count(df, x)
  expect_equal(out, tibble(x = c(1, 2), n = c(3, 2)))
})

test_that("count can sort output by `n`", {
  df <- tibble(x = c(1, 1, 2, 2, 2))
  out <- count(df, x, sort = TRUE)
  expect_equal(out, tibble(x = c(2, 1), n = c(3, 2)))
})

test_that("count can rename grouping columns", {
  # But should it really allow this?
  df <- tibble(x = c(2, 1, 1, 2, 1))
  out <- count(df, y = x)
  expect_equal(out, tibble(y = c(1, 2), n = c(3, 2)))
})

test_that("informs if n column already present, unless overridden", {
  df1 <- tibble(n = c(1, 1, 2, 2, 2))
  expect_message(out <- count(df1, n), "already present")
  expect_named(out, c("n", "nn"))

  # not a good idea, but supported
  expect_message(out <- count(df1, n, name = "n"), NA)
  expect_named(out, "n")

  expect_message(out <- count(df1, n, name = "nn"), NA)
  expect_named(out, c("n", "nn"))

  df2 <- tibble(n = c(1, 1, 2, 2, 2), nn = 1:5)
  expect_message(out <- count(df2, n), "already present")
  expect_named(out, c("n", "nn"))

  expect_message(out <- count(df2, n, nn), "already present")
  expect_named(out, c("n", "nn", "nnn"))
})

test_that("name must be string", {
  df <- tibble(x = c(1, 2))
  expect_snapshot(error = TRUE, count(df, x, name = 1))
  expect_snapshot(error = TRUE, count(df, x, name = letters))
})

test_that("output includes empty levels with .drop = FALSE", {
  df <- tibble(f = factor("b", levels = c("a", "b", "c")))
  out <- count(df, f, .drop = FALSE)
  expect_equal(out$n, c(0, 1, 0))

  out <- count(group_by(df, f, .drop = FALSE))
  expect_equal(out$n, c(0, 1, 0))
})

test_that("count preserves grouping", {
  df <- tibble(g = c(1, 2, 2, 2))
  exp <- tibble(g = c(1, 2), n = c(1, 3))

  expect_equal(df %>% count(g), exp)
  expect_equal(df %>% group_by(g) %>% count(), exp %>% group_by(g))
})

test_that("output preserves class & attributes where possible", {
  df <- data.frame(g = c(1, 2, 2, 2))
  attr(df, "my_attr") <- 1

  out <- df %>% count(g)
  expect_s3_class(out, "data.frame", exact = TRUE)
  expect_equal(attr(out, "my_attr"), 1)

  out <- df %>% group_by(g) %>% count()
  expect_s3_class(out, "grouped_df")
  expect_equal(group_vars(out), "g")
  # summarise() currently drops attributes
  expect_null(attr(out, "my_attr"))
})

test_that("works with dbplyr", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("RSQLite")

  db <- dbplyr::memdb_frame(x = c(1, 1, 1, 2, 2))
  df1 <- db %>% count(x) %>% as_tibble()
  expect_equal(df1, tibble(x = c(1, 2), n = c(3, 2)))

  df2 <- db %>% add_count(x) %>% as_tibble()
  expect_equal(df2, tibble(x = c(1, 1, 1, 2, 2), n = c(3, 3, 3, 2, 2)))
})

test_that("dbplyr `count()` method has transient internal grouping (#6338, tidyverse/dbplyr#940)", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("RSQLite")

  db <- dbplyr::memdb_frame(
    x = c(1, 1, 1, 2, 2),
    y = c("a", "a", "b", "c", "c")
  )

  df <- db %>%
    count(x, y) %>%
    collect()

  expect <- tibble(
    x = c(1, 1, 2),
    y = c("a", "b", "c"),
    n = c(2L, 1L, 2L)
  )

  expect_false(is_grouped_df(df))
  expect_identical(df, expect)
})

test_that("can only explicitly chain together multiple tallies", {
  expect_snapshot({
    df <- data.frame(g = c(1, 1, 2, 2), n = 1:4)

    df %>% count(g, wt = n)
    df %>% count(g, wt = n) %>% count(wt = n)
    df %>% count(n)
  })
})

test_that("wt = n() is deprecated", {
  df <- data.frame(x = 1:3)
  expect_warning(count(df, wt = n()), "`wt = n()`", fixed = TRUE)
})

test_that("count() owns errors (#6139)", {
  expect_snapshot({
    (expect_error(count(mtcars, new = 1 + "")))
    (expect_error(count(mtcars, wt = 1 + "")))
  })
})

# tally -------------------------------------------------------------------

test_that("tally can sort output", {
  gf <- group_by(tibble(x = c(1, 1, 2, 2, 2)), x)
  out <- tally(gf, sort = TRUE)
  expect_equal(out, tibble(x = c(2, 1), n = c(3, 2)))
})

test_that("weighted tally drops NAs (#1145)", {
  df <- tibble(x = c(1, 1, NA))
  expect_equal(tally(df, x)$n, 2)
})

test_that("tally() drops last group (#5199) ", {
  df <- data.frame(x = 1, y = 2, z = 3)

  res <- expect_message(df %>% group_by(x, y) %>% tally(wt = z), NA)
  expect_equal(group_vars(res), "x")
})

test_that("tally() owns errors (#6139)", {
  expect_snapshot({
    (expect_error(tally(mtcars, wt = 1 + "")))
  })
})

# add_count ---------------------------------------------------------------

test_that("add_count preserves grouping", {
  df <- tibble(g = c(1, 2, 2, 2))
  exp <- tibble(g = c(1, 2, 2, 2), n = c(1, 3, 3, 3))

  expect_equal(df %>% add_count(g), exp)
  expect_equal(df %>% group_by(g) %>% add_count(), exp %>% group_by(g))
})

test_that(".drop is deprecated",  {
  local_options(lifecycle_verbosity = "warning")

  df <- tibble(f = factor("b", levels = c("a", "b", "c")))
  expect_warning(out <- add_count(df, f, .drop = FALSE), "deprecated")
})

test_that("add_count() owns errors (#6139)", {
  expect_snapshot({
    (expect_error(add_count(mtcars, new = 1 + "")))
    (expect_error(add_count(mtcars, wt = 1 + "")))
  })
})

# add_tally ---------------------------------------------------------------

test_that("can add tallies of a variable", {
  df <- tibble(a = c(2, 1, 1))
  expect_equal(
    df %>% group_by(a) %>% add_tally(),
    group_by(tibble(a = c(2, 1, 1), n = c(1, 2, 2)), a)
  )
})

test_that("add_tally can be given a weighting variable", {
  df <- data.frame(a = c(1, 1, 2, 2, 2), w = c(1, 1, 2, 3, 4))

  out <- df %>% group_by(a) %>% add_tally(wt = w)
  expect_equal(out$n, c(2, 2, 9, 9, 9))

  out <- df %>% group_by(a) %>% add_tally(wt = w + 1)
  expect_equal(out$n, c(4, 4, 12, 12, 12))
})

test_that("can override output column", {
  df <- data.frame(g = c(1, 1, 2, 2, 2), x = c(3, 2, 5, 5, 5))
  expect_named(add_tally(df, name = "xxx"), c("g", "x", "xxx"))
})

test_that("add_tally() owns errors (#6139)", {
  expect_snapshot({
    (expect_error(add_tally(mtcars, wt = 1 + "")))
  })
})
