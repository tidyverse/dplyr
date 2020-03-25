setup(options(lifecycle_verbosity = "quiet"))
teardown(options(lifecycle_verbosity = NULL))

test_that("can select negatively (#2519)", {
  expect_identical(select_(mtcars, ~ -cyl), mtcars[-2])
})

test_that("select yields proper names", {
  expect_identical(names(select_(mtcars, ~ cyl:hp)), c("cyl", "disp", "hp"))
})

test_that("lazydots are named and arrange() doesn't fail (it assumes empty names)", {
  dots <- compat_lazy_dots(list(), env(), "cyl")
  expect_identical(names(dots), "")
  expect_identical(arrange_(mtcars, "cyl"), arrange(mtcars, cyl))
})

test_that("mutate_each_() and summarise_each_() handle lazydots", {
  cyl_chr <- mutate_each_(mtcars, list(as.character), "cyl")$cyl
  expect_identical(cyl_chr, as.character(mtcars$cyl))

  cyl_mean <- summarise_each_(mtcars, list(mean), "cyl")$cyl
  expect_equal(cyl_mean, mean(mtcars$cyl))
})

test_that("select_vars_() handles lazydots", {
  expect_identical(select_vars_(letters, c("a", "b")), set_names(c("a", "b")))
})

df <- tibble(
  a = c(1:3, 2:3),
  b = letters[c(1:4, 4L)]
)

test_that("arrange_ works", {
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
})

test_that("mutate_ works", {
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
  expect_equal(
    summarise_(df, a = ~ mean(a)),
    summarise(df, a = mean(a))
  )

  expect_equal(
    summarise_(df, .dots = list(a = quote(mean(a)))),
    summarise(df, a = mean(a))
  )

  expect_equal(
    summarise_(df, .dots = list(a = ~ mean(a))),
    summarise(df, a = mean(a))
  )

  my_mean <- mean
  expect_identical(
    summarise_(df, .dots = c(a = "my_mean(a)")),
    summarise(df, a = my_mean(a))
  )

  expect_equal(
    summarise_(df %>% group_by(b), a = ~ mean(a)),
    summarise(df %>% group_by(b), a = mean(a))
  )

  expect_equal(
    summarise_(df %>% group_by(b), .dots = list(a = quote(mean(a)))),
    summarise(df %>% group_by(b), a = mean(a))
  )

  expect_equal(
    summarise_(df %>% group_by(b), .dots = list(a = ~ mean(a))),
    summarise(df %>% group_by(b), a = mean(a))
  )
})

test_that("summarize_ works", {
  expect_equal(
    summarize_(df, a = ~ mean(a)),
    summarize(df, a = mean(a))
  )

  expect_equal(
    summarize_(df, .dots = list(a = quote(mean(a)))),
    summarize(df, a = mean(a))
  )

  expect_equal(
    summarize_(df, .dots = list(a = ~ mean(a))),
    summarize(df, a = mean(a))
  )

  expect_equal(
    summarize_(df %>% group_by(b), a = ~ mean(a)),
    summarize(df %>% group_by(b), a = mean(a))
  )

  expect_equal(
    summarize_(df %>% group_by(b), .dots = list(a = quote(mean(a)))),
    summarize(df %>% group_by(b), a = mean(a))
  )

  expect_equal(
    summarize_(df %>% group_by(b), .dots = list(a = ~ mean(a))),
    summarize(df %>% group_by(b), a = mean(a))
  )
})

test_that("transmute_ works", {
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


test_that("group_by_ backwards compatibility with add = TRUE adds groups", {
  df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
  add_groups_extendedclass <- function(tbl) {
    grouped <- group_by(tbl, x)
    group_by.default(grouped, y, add = TRUE)
  }

  expect_equal(group_vars(add_groups_extendedclass(df)), c("x", "y"))
})

test_that("_each() and _all() families agree", {
  df <- data.frame(x = 1:3, y = 1:3)

  expect_equal(summarise_each(df, list(mean)), summarise_all(df, mean))
  expect_equal(summarise_each(df, list(mean), x), summarise_at(df, vars(x), mean))
  expect_equal(summarise_each(df, list(mean = mean), x), summarise_at(df, vars(x), list(mean = mean)))
  expect_equal(summarise_each(df, list(mean = mean), x:y), summarise_at(df, vars(x:y), list(mean = mean)))
  expect_equal(summarise_each(df, list(mean), x:y), summarise_at(df, vars(x:y), mean))
  expect_equal(summarise_each(df, list(mean), z = y), summarise_at(df, vars(z = y), mean))

  expect_equal(mutate_each(df, list(mean)), mutate_all(df, mean))
  expect_equal(mutate_each(df, list(mean), x), mutate_at(df, vars(x), mean))
  expect_equal(mutate_each(df, list(mean = mean), x), mutate_at(df, vars(x), list(mean = mean)))
  expect_equal(mutate_each(df, list(mean = mean), x:y), mutate_at(df, vars(x:y), list(mean = mean)))
  expect_equal(mutate_each(df, list(mean), x:y), mutate_at(df, vars(x:y), mean))
  expect_equal(mutate_each(df, list(mean), z = y), mutate_at(df, vars(z = y), mean))
})
