context("count-tally")

# count -------------------------------------------------------------------

test_that("must manually supply name when n column already present", {
  df <- data.frame(n = c(1, 1, 2, 2, 2))
  expect_named(count(df, n, name = "nn"), c("n", "nn"))

  df <- data.frame(g = c(1, 2, 2, 2), n = c(1:4))
  expect_named(count(df, g, name = "n"), c("g", "n"))
})

test_that("count preserves type of input", {
  df <- data.frame(g = c(1, 2, 2, 2))
  attr(df, "my_attr") <- 1

  out <- df %>% count(g)
  expect_s3_class(out, "data.frame", exact = TRUE)
  expect_equal(attr(out, "my_attr"), 1)

  out <- df %>% group_by(g) %>% count()
  expect_s3_class(out, "grouped_df")
  expect_equal(group_vars(out), "g")
  # TODO: currently lost by summarise
  # expect_equal(attr(out, "my_attr"), 1)
})

test_that("grouped count includes group", {
  df <- data.frame(g = c(1, 2, 2, 2))

  res <- df %>% group_by(g) %>% count()
  expect_equal(names(res), c("g", "n"))
  expect_equal(res$n, c(1, 3))
  expect_equal(group_vars(res), "g")
})

test_that("can override variable name", {
  df <- data.frame(g = c(1, 1, 2, 2, 2))
  expect_named(count(df, g, name = "xxx"), c("g", "xxx"))
})

test_that("count() preserves with .drop", {
  df <- tibble(f = factor("b", levels = c("a", "b", "c")))
  out <- count(df, f, .drop = FALSE)
  expect_equal(out$n, c(0, 1, 0))

  out <- count(group_by(df, f, .drop = FALSE))
  expect_equal(out$n, c(0, 1, 0))
})

test_that("works with dbplyr", {
  db <- dbplyr::memdb_frame(x = c(1, 1, 1, 2, 2))
  df1 <- db %>% count(x) %>% as_tibble()
  expect_equal(df1, tibble(x = c(1, 2), n = c(3, 2)))

  df2 <- db %>% add_count(x) %>% as_tibble()
  expect_equal(df2, tibble(x = c(1, 1, 1, 2, 2), n = c(3, 3, 3, 2, 2)))
})

# add_count ---------------------------------------------------------------

test_that("must manually supply name when n column already present", {
  df <- data.frame(n = c(1, 1, 2, 2, 2))
  expect_named(add_count(df, n, name = "nn"), c("n", "nn"))
})

test_that("can override output column", {
  df <- data.frame(g = c(1, 1, 2, 2, 2), x = c(3, 2, 5, 5, 5))
  expect_named(add_count(df, g, name = "xxx"), c("g", "x", "xxx"))
})

test_that(".drop is deprecated",  {
  df <- tibble(f = factor("b", levels = c("a", "b", "c")))
  expect_warning(out <- add_count(df, f, .drop = FALSE), "deprecated")
})

# tally -------------------------------------------------------------------

test_that("weighted tally drops NAs (#1145)", {
  df <- tibble(x = c(1, 1, NA))
  expect_equal(tally(df, x)$n, 2)
})

test_that("can override output column", {
  df <- data.frame(g = c(1, 1, 2, 2, 2), x = c(3, 2, 5, 5, 5))
  expect_named(tally(df, name = "xxx"), "xxx")
})

test_that("tally uses variable named n as default wt.", {
  df <- tibble(n = 1:3)
  expect_message(res <- df %>% tally(name = "nn"), "Using `n` as weighting variable")
  expect_named(res, "nn")
})

test_that("tally() does .groups = 'drop_last' (#5199) ", {
  res <- expect_message(
    data.frame(x = 1, y = 2, z = 3) %>%
      group_by(x, y) %>%
      tally(wt = z),
    NA)
  expect_equal(group_vars(res), "x")
})

# add_tally ---------------------------------------------------------------

test_that("can add tallies of a variable", {
  df <- data.frame(a = c(1, 1, 2, 2, 2))

  out <- df %>% group_by(a) %>% add_tally()
  expect_equal(names(out), c("a", "n"))
  expect_equal(out$a, df$a)
  expect_equal(out$n, c(2, 2, 3, 3, 3))

  out <- df %>% group_by(a) %>% add_tally(sort = TRUE)
  expect_equal(out$n, c(3, 3, 3, 2, 2))
})

test_that("add_tally respects and preserves existing groups", {
  df <- data.frame(g = c(1, 2, 2, 2), val = c("b", "b", "b", "c"))
  res <- df %>% group_by(val) %>% add_tally()
  expect_equal(res$n, c(3, 3, 3, 1))
  expect_equal(group_vars(res), "val")

  res <- df %>% group_by(g, val) %>% add_tally()
  expect_equal(res$n, c(1, 2, 2, 1))
  expect_equal(group_vars(res), c("g", "val"))
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


# Errors ------------------------------------------------------------------

test_that("count() give meaningful errors", {
  verify_output(test_path("test-count-tally-errors.txt"), {
    df <- data.frame(n = c(1, 1, 2, 2, 2))
    add_count(df, n)

    df <- data.frame(g = c(1, 2, 2, 2), n = c(1:4))
    count(df, g)

    df <- data.frame(n = c(1, 1, 2, 2, 2))
    count(df, n)
  })
})
