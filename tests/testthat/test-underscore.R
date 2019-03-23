context("underscore")

df <- tibble(
  a = c(1:3, 2:3),
  b = letters[c(1:4, 4L)]
)

test_that("arrange_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    arrange_(df, ~ -a),
    arrange(df, -a)
  )

  expect_equal(
    arrange_(df, .dots = list(quote(-a))),
    arrange(df, -a)
  )

  expect_equal(
    arrange_(df, .dots = list(~ -a)),
    arrange(df, -a)
  )
})

test_that("count_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    count_(df, ~ b),
    count(df, b)
  )

  expect_equal(
    count_(df, ~ b, wt = quote(a)),
    count(df, b, wt = a)
  )

  wt <- 1:4
  expect_identical(
    count_(df, "b", "wt"),
    count(df, b, wt = wt)
  )

  expect_identical(
    add_count(df, b),
    add_count_(df, ~ b)
  )
})

test_that("distinct_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    distinct_(df, ~ a),
    distinct(df, a)
  )

  expect_equal(
    distinct_(df, .dots = list(quote(a))),
    distinct(df, a)
  )

  expect_equal(
    distinct_(df, .dots = list(~ a)),
    distinct(df, a)
  )

  expect_equal(
    distinct_(df %>% group_by(b), ~ a, .dots = NULL),
    distinct(df %>% group_by(b), a)
  )

  expect_equal(
    distinct_(df %>% group_by(b), .dots = list(quote(a))),
    distinct(df %>% group_by(b), a)
  )

  expect_equal(
    distinct_(df %>% group_by(b), .dots = list(~ a)),
    distinct(df %>% group_by(b), a)
  )
})

test_that("do_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    do_(df, ~ tibble(-.$a)),
    do(df, tibble(-.$a))
  )

  expect_equal(
    do_(df, .dots = list(quote(dplyr::tibble(-.$a)))),
    do(df, tibble(-.$a))
  )

  expect_equal(
    do_(df, .dots = list(~ dplyr::tibble(-.$a))),
    do(df, tibble(-.$a))
  )

  foo <- "foobar"
  expect_identical(
    do_(df, .dots = "tibble(foo)"),
    do(df, tibble(foo))
  )

  expect_equal(
    do_(df %>% group_by(b), ~ tibble(-.$a)),
    do(df %>% group_by(b), tibble(-.$a))
  )

  expect_equal(
    do_(df %>% group_by(b), .dots = list(quote(dplyr::tibble(-.$a)))),
    do(df %>% group_by(b), tibble(-.$a))
  )

  expect_equal(
    do_(df %>% group_by(b), .dots = list(~ dplyr::tibble(-.$a))),
    do(df %>% group_by(b), tibble(-.$a))
  )
})

test_that("filter_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    filter_(df, ~ a > 1),
    filter(df, a > 1)
  )

  expect_equal(
    filter_(df, .dots = list(quote(a > 1))),
    filter(df, a > 1)
  )

  cnd <- rep(TRUE, 5)
  expect_identical(
    filter_(df, .dots = "cnd"),
    filter(df, cnd)
  )
})

test_that("group_by_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    group_by_(df, ~ a),
    group_by(df, a)
  )

  expect_equal(
    group_by_(df, ~ -a),
    group_by(df, -a)
  )

  expect_equal(
    group_by_(df, .dots = "a"),
    group_by(df, a)
  )

  expect_equal(
    group_by_(df, .dots = list(quote(-a))),
    group_by(df, -a)
  )

  expect_equal(
    group_by_(df, .dots = list(~ -a)),
    group_by(df, -a)
  )

  expect_warning(
    expect_equal(
      group_by_(df %>% rowwise(), ~ a),
      group_by(df %>% rowwise(), a)
    ),
    "rowwise"
  )

  expect_warning(
    expect_equal(
      group_by_(df %>% rowwise(), ~ -a),
      group_by(df %>% rowwise(), -a)
    ),
    "rowwise"
  )

  expect_warning(
    expect_equal(
      group_by_(df %>% rowwise(), .dots = "a"),
      group_by(df %>% rowwise(), a)
    ),
    "rowwise"
  )

  expect_warning(
    expect_equal(
      group_by_(df %>% rowwise(), .dots = list(quote(-a))),
      group_by(df %>% rowwise(), -a)
    ),
    "rowwise"
  )

  expect_warning(
    expect_equal(
      group_by_(df %>% rowwise(), .dots = list(~ -a)),
      group_by(df %>% rowwise(), -a)
    ),
    "rowwise"
  )
})

test_that("mutate_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    mutate_(df, c = ~ -a),
    mutate(df, c = -a)
  )

  expect_equal(
    mutate_(df, .dots = list(c = quote(-a))),
    mutate(df, c = -a)
  )

  expect_equal(
    mutate_(df, .dots = list(c = ~ -a)),
    mutate(df, c = -a)
  )

  expect_identical(
    mutate_(df, ~ -a),
    mutate(df, -a)
  )

  foo <- "foobar"
  expect_identical(
    mutate_(df, .dots = "foo"),
    mutate(df, foo)
  )
})

test_that("rename_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    rename_(df, c = ~ a),
    rename(df, c = a)
  )

  expect_equal(
    rename_(df, .dots = list(c = quote(a))),
    rename(df, c = a)
  )

  expect_equal(
    rename_(df, .dots = list(c = ~ a)),
    rename(df, c = a)
  )
})

test_that("select_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    select_(df, ~ a),
    select(df, a)
  )

  expect_equal(
    select_(df, ~ -a),
    select(df, -a)
  )

  expect_equal(
    select_(df, .dots = "a"),
    select(df, a)
  )

  expect_equal(
    select_(df, .dots = list(quote(-a))),
    select(df, -a)
  )

  expect_equal(
    select_(df, .dots = list(~ -a)),
    select(df, -a)
  )

  pos <- 1
  expect_identical(
    select_(df, c = "pos"),
    select(df, c = pos)
  )
})

test_that("slice_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    slice_(df, ~ 2:n()),
    slice(df, 2:n())
  )

  expect_equal(
    slice_(df, .dots = list(quote(2:n()))),
    slice(df, 2:n())
  )

  expect_equal(
    slice_(df, .dots = list(~ 2:n())),
    slice(df, 2:n())
  )

  pos <- 3
  expect_identical(
    slice_(df, .dots = "pos:n()"),
    slice(df, pos:n())
  )
})

test_that("summarise_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    summarise_(df, ~ mean(a)),
    summarise(df, mean(a))
  )

  expect_equal(
    summarise_(df, .dots = list(quote(mean(a)))),
    summarise(df, mean(a))
  )

  expect_equal(
    summarise_(df, .dots = list(~ mean(a))),
    summarise(df, mean(a))
  )

  my_mean <- mean
  expect_identical(
    summarise_(df, .dots = "my_mean(a)"),
    summarise(df, my_mean(a))
  )

  expect_equal(
    summarise_(df %>% group_by(b), ~ mean(a)),
    summarise(df %>% group_by(b), mean(a))
  )

  expect_equal(
    summarise_(df %>% group_by(b), .dots = list(quote(mean(a)))),
    summarise(df %>% group_by(b), mean(a))
  )

  expect_equal(
    summarise_(df %>% group_by(b), .dots = list(~ mean(a))),
    summarise(df %>% group_by(b), mean(a))
  )
})

test_that("summarize_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    summarize_(df, ~ mean(a)),
    summarize(df, mean(a))
  )

  expect_equal(
    summarize_(df, .dots = list(quote(mean(a)))),
    summarize(df, mean(a))
  )

  expect_equal(
    summarize_(df, .dots = list(~ mean(a))),
    summarize(df, mean(a))
  )

  expect_equal(
    summarize_(df %>% group_by(b), ~ mean(a)),
    summarize(df %>% group_by(b), mean(a))
  )

  expect_equal(
    summarize_(df %>% group_by(b), .dots = list(quote(mean(a)))),
    summarize(df %>% group_by(b), mean(a))
  )

  expect_equal(
    summarize_(df %>% group_by(b), .dots = list(~ mean(a))),
    summarize(df %>% group_by(b), mean(a))
  )
})

test_that("transmute_ works", {
  scoped_lifecycle_silence()
  expect_equal(
    transmute_(df, c = ~ -a),
    transmute(df, c = -a)
  )

  expect_equal(
    transmute_(df, .dots = list(c = quote(-a))),
    transmute(df, c = -a)
  )

  expect_equal(
    transmute_(df, .dots = list(c = ~ -a)),
    transmute(df, c = -a)
  )

  foo <- "foobar"
  expect_identical(
    transmute_(df, .dots = "foo"),
    transmute(df, foo)
  )
})
