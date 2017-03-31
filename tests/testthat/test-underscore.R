context("underscore")

df <- data_frame(
  a = c(1:3, 2:3),
  b = letters[c(1:4, 4L)]
)

test_that("arrange_ works", {
  expect_equal(
    arrange_(df, ~-a),
    arrange(df, -a)
  )

  expect_equal(
    arrange_(df, .dots = list(quote(-a))),
    arrange(df, -a)
  )

  expect_equal(
    arrange_(df, .dots = list(~-a)),
    arrange(df, -a)
  )
})

test_that("count_ works", {
  expect_equal(
    count_(df, ~b),
    count(df, b)
  )

  expect_equal(
    count_(df, ~b, wt = quote(a)),
    count(df, b, wt = a)
  )

  expect_identical(
    add_count(df, b),
    add_count_(df, ~b)
  )
})

test_that("distinct_ works", {
  expect_equal(
    distinct_(df, ~a),
    distinct(df, a)
  )

  expect_equal(
    distinct_(df, .dots = list(quote(a))),
    distinct(df, a)
  )

  expect_equal(
    distinct_(df, .dots = list(~a)),
    distinct(df, a)
  )

  expect_equal(
    distinct_(df %>% group_by(b), ~a, .dots = NULL),
    distinct(df %>% group_by(b), a)
  )

  expect_equal(
    distinct_(df %>% group_by(b), .dots = list(quote(a))),
    distinct(df %>% group_by(b), a)
  )

  expect_equal(
    distinct_(df %>% group_by(b), .dots = list(~a)),
    distinct(df %>% group_by(b), a)
  )
})

test_that("do_ works", {
  expect_equal(
    do_(df, ~data_frame(-.$a)),
    do(df, data_frame(-.$a))
  )

  expect_equal(
    do_(df, .dots = list(quote(dplyr::data_frame(-.$a)))),
    do(df, data_frame(-.$a))
  )

  expect_equal(
    do_(df, .dots = list(~dplyr::data_frame(-.$a))),
    do(df, data_frame(-.$a))
  )

  expect_equal(
    do_(df %>% group_by(b), ~data_frame(-.$a)),
    do(df %>% group_by(b), data_frame(-.$a))
  )

  expect_equal(
    do_(df %>% group_by(b), .dots = list(quote(dplyr::data_frame(-.$a)))),
    do(df %>% group_by(b), data_frame(-.$a))
  )

  expect_equal(
    do_(df %>% group_by(b), .dots = list(~dplyr::data_frame(-.$a))),
    do(df %>% group_by(b), data_frame(-.$a))
  )
})

test_that("filter_ works", {
  expect_equal(
    filter_(df, ~a > 1),
    filter(df, a > 1)
  )

  expect_equal(
    filter_(df, .dots = list(quote(a > 1))),
    filter(df, a > 1)
  )
})

test_that("funs_ works", {
  expect_equal(
    funs(mean),
    funs_(list(~mean))
  )

  expect_equal(
    funs_(list("mean")),
    funs_(list(`environment<-`(~mean, baseenv())))
  )

  expect_equal(
    funs(mean(.)),
    funs_(list(~mean(.)))
  )
})

test_that("group_by_ works", {
  expect_equal(
    group_by_(df, ~a),
    group_by(df, a)
  )

  expect_equal(
    group_by_(df, ~-a),
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
    group_by_(df, .dots = list(~-a)),
    group_by(df, -a)
  )

  expect_warning(
    expect_equal(
      group_by_(df %>% rowwise, ~a),
      group_by(df %>% rowwise, a)
    ),
    "rowwise"
  )

  expect_warning(
    expect_equal(
      group_by_(df %>% rowwise, ~-a),
      group_by(df %>% rowwise, -a)
    ),
    "rowwise"
  )

  expect_warning(
    expect_equal(
      group_by_(df %>% rowwise, .dots = "a"),
      group_by(df %>% rowwise, a)
    ),
    "rowwise"
  )

  expect_warning(
    expect_equal(
      group_by_(df %>% rowwise, .dots = list(quote(-a))),
      group_by(df %>% rowwise, -a)
    ),
    "rowwise"
  )

  expect_warning(
    expect_equal(
      group_by_(df %>% rowwise, .dots = list(~-a)),
      group_by(df %>% rowwise, -a)
    ),
    "rowwise"
  )
})

test_that("mutate_ works", {
  expect_equal(
    mutate_(df, c = ~-a),
    mutate(df, c = -a)
  )

  expect_equal(
    mutate_(df, .dots = list(c = quote(-a))),
    mutate(df, c = -a)
  )

  expect_equal(
    mutate_(df, .dots = list(c = ~-a)),
    mutate(df, c = -a)
  )
})

test_that("rename_ works", {
  expect_equal(
    rename_(df, c = ~a),
    rename(df, c = a)
  )

  expect_equal(
    rename_(df, .dots = list(c = quote(a))),
    rename(df, c = a)
  )

  expect_equal(
    rename_(df, .dots = list(c = ~a)),
    rename(df, c = a)
  )
})

test_that("select_ works", {
  expect_equal(
    select_(df, ~a),
    select(df, a)
  )

  expect_equal(
    select_(df, ~-a),
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
    select_(df, .dots = list(~-a)),
    select(df, -a)
  )
})

test_that("slice_ works", {
  expect_equal(
    slice_(df, ~2:n()),
    slice(df, 2:n())
  )

  expect_equal(
    slice_(df, .dots = list(quote(2:n()))),
    slice(df, 2:n())
  )

  expect_equal(
    slice_(df, .dots = list(~2:n())),
    slice(df, 2:n())
  )
})

test_that("summarise_ works", {
  expect_equal(
    summarise_(df, ~mean(a)),
    summarise(df, mean(a))
  )

  expect_equal(
    summarise_(df, .dots = list(quote(mean(a)))),
    summarise(df, mean(a))
  )

  expect_equal(
    summarise_(df, .dots = list(~mean(a))),
    summarise(df, mean(a))
  )

  expect_equal(
    summarise_(df %>% group_by(b), ~mean(a)),
    summarise(df %>% group_by(b), mean(a))
  )

  expect_equal(
    summarise_(df %>% group_by(b), .dots = list(quote(mean(a)))),
    summarise(df %>% group_by(b), mean(a))
  )

  expect_equal(
    summarise_(df %>% group_by(b), .dots = list(~mean(a))),
    summarise(df %>% group_by(b), mean(a))
  )
})

test_that("summarize_ works", {
  expect_equal(
    summarize_(df, ~mean(a)),
    summarize(df, mean(a))
  )

  expect_equal(
    summarize_(df, .dots = list(quote(mean(a)))),
    summarize(df, mean(a))
  )

  expect_equal(
    summarize_(df, .dots = list(~mean(a))),
    summarize(df, mean(a))
  )

  expect_equal(
    summarize_(df %>% group_by(b), ~mean(a)),
    summarize(df %>% group_by(b), mean(a))
  )

  expect_equal(
    summarize_(df %>% group_by(b), .dots = list(quote(mean(a)))),
    summarize(df %>% group_by(b), mean(a))
  )

  expect_equal(
    summarize_(df %>% group_by(b), .dots = list(~mean(a))),
    summarize(df %>% group_by(b), mean(a))
  )
})

test_that("transmute_ works", {
  expect_equal(
    transmute_(df, c = ~-a),
    transmute(df, c = -a)
  )

  expect_equal(
    transmute_(df, .dots = list(c = quote(-a))),
    transmute(df, c = -a)
  )

  expect_equal(
    transmute_(df, .dots = list(c = ~-a)),
    transmute(df, c = -a)
  )
})
