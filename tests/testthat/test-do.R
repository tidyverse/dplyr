context("Do")

# Grouped data frames ----------------------------------------------------------

df <- data.frame(
  g = c(1, 2, 2, 3, 3, 3),
  x = 1:6,
  y = 6:1
) %>% group_by(g)

test_that("can't use both named and unnamed args", {
  expect_error(
    df %>% do(x = 1, 2),
    "Arguments must either be all named or all unnamed",
    fixed = TRUE
  )
})

test_that("unnamed elements must return data frames", {
  expect_error(
    df %>% ungroup() %>% do(1), "Result must be a data frame, not numeric"
  )
  expect_error(
    df %>% do(1), "Results 1, 2, 3 must be data frames, not numeric"
  )
  expect_error(
    df %>% do("a"), "Results 1, 2, 3 must be data frames, not character"
  )
})

test_that("unnamed results bound together by row", {
  first <- df %>% do(head(., 1))

  expect_equal(nrow(first), 3)
  expect_equal(first$g, 1:3)
  expect_equal(first$x, c(1, 2, 4))
})

test_that("can only use single unnamed argument", {
  expect_error(
    df %>% do(head, tail),
    "Can only supply one unnamed argument, not 2"
  )
})

test_that("named argument become list columns", {
  out <- df %>% do(nrow = nrow(.), ncol = ncol(.))
  expect_equal(out$nrow, list(1, 2, 3))
  # includes grouping columns
  expect_equal(out$ncol, list(3, 3, 3))
})

test_that("multiple outputs can access data (#2998)", {
  out <- do(tibble(a = 1), g = nrow(.), h = nrow(.))
  expect_equal(names(out), c("g", "h"))
  expect_equal(out$g, list(1L))
  expect_equal(out$h, list(1L))
})

test_that("colums in output override columns in input", {
  out <- df %>% do(data.frame(g = 1))
  expect_equal(names(out), "g")
  expect_equal(out$g, c(1, 1, 1))
})

test_that("empty results preserved (#597)", {
  blankdf <- function(x) data.frame(blank = numeric(0))

  dat <- data.frame(a = 1:2, b = factor(1:2))
  expect_equal(
    dat %>% group_by(b, .drop = FALSE) %>% do(blankdf(.)),
    data.frame(b = factor(integer(), levels = 1:2), blank = numeric())
  )
})

test_that("empty inputs give empty outputs (#597)", {
  out <- data.frame(a = numeric(), b = factor()) %>%
    group_by(b, .drop = FALSE) %>%
    do(data.frame())
  expect_equal(out, data.frame(b = factor()) %>% group_by(b, .drop = FALSE))

  out <- data.frame(a = numeric(), b = character()) %>%
    group_by(b, .drop = FALSE) %>%
    do(data.frame())
  expect_equal(out, data.frame(b = character()) %>% group_by(b, .drop = FALSE))
})

test_that("grouped do evaluates args in correct environment", {
  a <- 10
  f <- function(a) {
    mtcars %>% group_by(cyl) %>% do(a = a)
  }
  expect_equal(f(100)$a, list(100, 100, 100))
})

# Ungrouped data frames --------------------------------------------------------

test_that("ungrouped data frame with unnamed argument returns data frame", {
  out <- mtcars %>% do(head(.))
  expect_is(out, "data.frame")
  expect_equal(dim(out), c(6, 11))
})

test_that("ungrouped data frame with named argument returns list data frame", {
  out <- mtcars %>% do(x = 1, y = 2:10)
  expect_is(out, "tbl_df")
  expect_equal(out$x, list(1))
  expect_equal(out$y, list(2:10))
})

test_that("ungrouped do evaluates args in correct environment", {
  a <- 10
  f <- function(a) {
    mtcars %>% do(a = a)
  }
  expect_equal(f(100)$a, list(100))
})

# Rowwise data frames ----------------------------------------------------------

test_that("can do on rowwise dataframe", {
  out <- mtcars %>% rowwise() %>% do(x = 1)
  exp <- tibble(x =rep(list(1), nrow(mtcars))) %>% rowwise()
  expect_identical(out, exp)
})


# Zero row inputs --------------------------------------------------------------

test_that("empty data frames give consistent outputs", {
  dat <- tibble(x = numeric(0), g = character(0))
  grp <- dat %>% group_by(g)
  emt <- grp %>% filter(FALSE)

  dat %>%
    do(data.frame()) %>%
    vapply(type_sum, character(1)) %>%
    length() %>%
    expect_equal(0)
  dat %>%
    do(data.frame(y = integer(0))) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(y = "int"))
  dat %>%
    do(data.frame(.)) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(x = "dbl", g = "chr"))
  dat %>%
    do(data.frame(., y = integer(0))) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(x = "dbl", g = "chr", y = "int"))
  dat %>%
    do(y = ncol(.)) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(y = "list"))

  # Grouped data frame should have same col types as ungrouped, with addition
  # of grouping variable
  grp %>%
    do(data.frame()) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(g = "chr"))
  grp %>%
    do(data.frame(y = integer(0))) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(g = "chr", y = "int"))
  grp %>%
    do(data.frame(.)) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(x = "dbl", g = "chr"))
  grp %>%
    do(data.frame(., y = integer(0))) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(x = "dbl", g = "chr", y = "int"))
  grp %>%
    do(y = ncol(.)) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(g = "chr", y = "list"))

  # A empty grouped dataset should have same types as grp
  emt %>%
    do(data.frame()) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(g = "chr"))
  emt %>%
    do(data.frame(y = integer(0))) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(g = "chr", y = "int"))
  emt %>%
    do(data.frame(.)) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(x = "dbl", g = "chr"))
  emt %>%
    do(data.frame(., y = integer(0))) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(x = "dbl", g = "chr", y = "int"))
  emt %>%
    do(y = ncol(.)) %>%
    vapply(type_sum, character(1)) %>%
    expect_equal(c(g = "chr", y = "list"))
})

test_that("handling of empty data frames in do", {
  blankdf <- function(x) data.frame(blank = numeric(0))
  dat <- data.frame(a = 1:2, b = factor(1:2))
  res <- dat %>% group_by(b, .drop = FALSE) %>% do(blankdf(.))
  expect_equal(names(res), c("b", "blank"))
})

test_that("do() does not retain .drop attribute (#4176)", {
  res <- iris %>%
    group_by(Species) %>%
    do(data.frame(n=1))
  expect_null(attr(res, ".drop", exact = TRUE))
})
