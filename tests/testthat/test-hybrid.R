context("hybrid")

# test_that("hybrid evaluation environment is cleaned up (#2358)", {
#   # Can't use pipe here, f and g should have top-level parent.env()
#   df <- data_frame(a = 1)
#   df <- mutate(df, f = {
#     a
#     list(function() {})
#   })
#   df <- mutate(df, g = {
#     f
#     list(quo(.))
#   })
#   df <- mutate(df, h = {
#     g
#     list(~ .)
#   })
#   df <- mutate(df, i = {
#     h
#     list(.data)
#   })
#
#   expect_true(env_has(df$f[[1]], "a", inherit = TRUE))
#   expect_true(env_has(df$g[[1]], "f", inherit = TRUE))
#   expect_true(env_has(df$h[[1]], "g", inherit = TRUE))
#
#   expect_warning(
#     expect_null(env_get(df$f[[1]], "a", inherit = TRUE)),
#     "Hybrid callback proxy out of scope",
#     fixed = TRUE
#   )
#   expect_warning(
#     expect_null(env_get(df$g[[1]], "f", inherit = TRUE)),
#     "Hybrid callback proxy out of scope",
#     fixed = TRUE
#   )
#   expect_warning(
#     expect_null(env_get(df$h[[1]], "g", inherit = TRUE)),
#     "Hybrid callback proxy out of scope",
#     fixed = TRUE
#   )
#   expect_warning(
#     expect_null(df$i[[1]]$h),
#     "Hybrid callback proxy out of scope",
#     fixed = TRUE
#   )
# })


test_that("n() and n_distinct() use hybrid evaluation", {
  d <- tibble(a = 1:5)
  expect_hybrid(d, n())
  expect_not_hybrid(d, list(1:n()))
  expect_not_hybrid(d, n() + 1)

  c <- 1:5
  expect_hybrid(d, n_distinct(a))
  expect_hybrid(d, n_distinct(a, na.rm = TRUE))
  expect_hybrid(d, n_distinct(a, na.rm = FALSE))
  expect_not_hybrid(d, n_distinct(c))
  expect_not_hybrid(d, n_distinct(a, c))

  d <- tibble(a = rep(1L, 3), b = 1:3)
  expect_hybrid(d, n_distinct(a, b))
  expect_hybrid(d, n_distinct(a, b, na.rm = TRUE))
  expect_hybrid(d, n_distinct(a, b, na.rm = FALSE))
  expect_not_hybrid(d, n_distinct())
})

test_that("<column> %in% <column> is hybrid", {
  d <- tibble(a = rep(1L, 3), b = 1:3)

  expect_hybrid(d, a %in% b)

  expect_not_hybrid(d, a %in% b[1])
  expect_not_hybrid(d, a[1] %in% b)
  expect_not_hybrid(d, a %in% 1:3)
})

test_that("min() and max() are hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))
  expect_hybrid(d, min(int))
  expect_hybrid(d, min(dbl))
  expect_not_hybrid(d, min(chr))

  expect_hybrid(d, min(int, na.rm = TRUE))
  expect_hybrid(d, min(dbl, na.rm = TRUE))
  expect_not_hybrid(d, min(int, na.rm = pi == pi))
  expect_not_hybrid(d, min(dbl, na.rm = pi == pi))
  expect_not_hybrid(d, min(dbl, na.rm = F))
  expect_not_hybrid(d, min(dbl, na.rm = T))
  expect_not_hybrid(d, min(chr, na.rm = TRUE))

  expect_hybrid(d, min(int, na.rm = FALSE))
  expect_hybrid(d, min(dbl, na.rm = FALSE))
  expect_not_hybrid(d, min(chr, na.rm = FALSE))

  expect_hybrid(d, max(int))
  expect_hybrid(d, max(dbl))
  expect_not_hybrid(d, max(chr))

  expect_hybrid(d, max(int, na.rm = TRUE))
  expect_hybrid(d, max(dbl, na.rm = TRUE))
  expect_not_hybrid(d, max(int, na.rm = pi == pi))
  expect_not_hybrid(d, max(dbl, na.rm = pi == pi))
  expect_not_hybrid(d, max(dbl, na.rm = F))
  expect_not_hybrid(d, max(dbl, na.rm = T))
  expect_not_hybrid(d, max(chr, na.rm = TRUE))

  expect_hybrid(d, max(int, na.rm = FALSE))
  expect_hybrid(d, max(dbl, na.rm = FALSE))
  expect_not_hybrid(d, max(chr, na.rm = FALSE))
})

test_that("first() and last() are hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, first(int))
  expect_hybrid(d, first(dbl))
  expect_hybrid(d, first(chr))

  expect_hybrid(d, first(int, default = 1L))
  expect_hybrid(d, first(dbl, default = 2))
  expect_hybrid(d, first(chr, default = ""))

  expect_hybrid(d, last(int))
  expect_hybrid(d, last(dbl))
  expect_hybrid(d, last(chr))

  expect_hybrid(d, last(int, default = 1L))
  expect_hybrid(d, last(dbl, default = 2))
  expect_hybrid(d, last(chr, default = ""))
})

test_that("nth(<column>, n = <int-ish>) is hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, nth(int, n = 1))
  expect_hybrid(d, nth(int, n = 1L))

  expect_hybrid(d, nth(dbl, n = 1))
  expect_hybrid(d, nth(dbl, n = 1L))

  expect_hybrid(d, nth(chr, n = 1))
  expect_hybrid(d, nth(chr, n = 1L))

})

test_that("nth(<column>, n = <int-ish>, default = <scalar>) is hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, nth(int, n = 1, default = 1L))
  expect_hybrid(d, nth(int, n = 1L, default = 1L))

  expect_hybrid(d, nth(dbl, n = 1, default = 1))
  expect_hybrid(d, nth(dbl, n = 1L, default = 1))

  expect_hybrid(d, nth(chr, n = 1, default = ""))
  expect_hybrid(d, nth(chr, n = 1L, default = ""))
})

test_that("lead() and lag() are hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, lead(int))
  expect_hybrid(d, lead(dbl))
  expect_hybrid(d, lead(chr))

  expect_hybrid(d, lead(int, n = 1))
  expect_hybrid(d, lead(dbl, n = 1))
  expect_hybrid(d, lead(chr, n = 1))

  expect_hybrid(d, lead(int, n = 1L))
  expect_hybrid(d, lead(dbl, n = 1L))
  expect_hybrid(d, lead(chr, n = 1L))
})

test_that("sum is hybrid", {
  d <- tibble(lgl = c(TRUE, FALSE), int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, sum(lgl))
  expect_hybrid(d, sum(int))
  expect_hybrid(d, sum(dbl))
  expect_not_hybrid(d, sum(chr))

  expect_hybrid(d, sum(lgl, na.rm = TRUE))
  expect_hybrid(d, sum(int, na.rm = TRUE))
  expect_hybrid(d, sum(dbl, na.rm = TRUE))
  expect_not_hybrid(d, sum(chr, na.rm = TRUE))

  expect_hybrid(d, sum(lgl, na.rm = FALSE))
  expect_hybrid(d, sum(int, na.rm = FALSE))
  expect_hybrid(d, sum(dbl, na.rm = FALSE))
  expect_not_hybrid(d, sum(chr, na.rm = FALSE))
})

test_that("mean is hybrid", {
  d <- tibble(lgl = c(TRUE, FALSE), int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, mean(lgl))
  expect_hybrid(d, mean(int))
  expect_hybrid(d, mean(dbl))
  expect_not_hybrid(d, mean(chr))

  expect_hybrid(d, mean(lgl, na.rm = TRUE))
  expect_hybrid(d, mean(int, na.rm = TRUE))
  expect_hybrid(d, mean(dbl, na.rm = TRUE))
  expect_not_hybrid(d, mean(chr, na.rm = TRUE))

  expect_hybrid(d, mean(lgl, na.rm = FALSE))
  expect_hybrid(d, mean(int, na.rm = FALSE))
  expect_hybrid(d, mean(dbl, na.rm = FALSE))
  expect_not_hybrid(d, mean(chr, na.rm = FALSE))
})

test_that("sd is hybrid", {
  d <- tibble(lgl = c(TRUE, FALSE), int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, sd(lgl))
  expect_hybrid(d, sd(int))
  expect_hybrid(d, sd(dbl))
  expect_not_hybrid(d, sd(chr))

  expect_hybrid(d, sd(lgl, na.rm = TRUE))
  expect_hybrid(d, sd(int, na.rm = TRUE))
  expect_hybrid(d, sd(dbl, na.rm = TRUE))
  expect_not_hybrid(d, sd(chr, na.rm = TRUE))

  expect_hybrid(d, sd(lgl, na.rm = FALSE))
  expect_hybrid(d, sd(int, na.rm = FALSE))
  expect_hybrid(d, sd(dbl, na.rm = FALSE))
  expect_not_hybrid(d, sd(chr, na.rm = FALSE))
})

test_that("var is hybrid", {
  d <- tibble(lgl = c(TRUE, FALSE), int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, var(lgl))
  expect_hybrid(d, var(int))
  expect_hybrid(d, var(dbl))
  expect_not_hybrid(d, var(chr))

  expect_hybrid(d, var(lgl, na.rm = TRUE))
  expect_hybrid(d, var(int, na.rm = TRUE))
  expect_hybrid(d, var(dbl, na.rm = TRUE))
  expect_not_hybrid(d, var(chr, na.rm = TRUE))

  expect_hybrid(d, var(lgl, na.rm = FALSE))
  expect_hybrid(d, var(int, na.rm = FALSE))
  expect_hybrid(d, var(dbl, na.rm = FALSE))
  expect_not_hybrid(d, var(chr, na.rm = FALSE))
})

test_that("row_number() is hybrid", {
  d <- tibble(a = 1:5)
  expect_hybrid(d, row_number())
})

test_that("ntile() is hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2))
  expect_hybrid(d, ntile(n = 2L))
  expect_hybrid(d, ntile(n = 2))

  expect_hybrid(d, ntile(int, n = 2L))
  expect_hybrid(d, ntile(int, n = 2))

  expect_hybrid(d, ntile(dbl, n = 2L))
  expect_hybrid(d, ntile(dbl, n = 2))
})

test_that("min_rank(), percent_rank(), dense_rank(), cume_dist() are hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2))

  expect_hybrid(d, min_rank(int))
  expect_hybrid(d, min_rank(dbl))

  expect_hybrid(d, percent_rank(int))
  expect_hybrid(d, percent_rank(dbl))

  expect_hybrid(d, dense_rank(int))
  expect_hybrid(d, dense_rank(dbl))

  expect_hybrid(d, cume_dist(int))
  expect_hybrid(d, cume_dist(dbl))
})

test_that("hybrid handlers don't nest", {
  d <- tibble(a = 1:5)
  expect_not_hybrid(d, mean(lag(a)))
  expect_not_hybrid(d, mean(row_number()))
  expect_not_hybrid(d, list(lag(cume_dist(a))))
})

test_that("simple handlers supports quosured symbols", {
  expect_hybrid(mtcars, mean(!!quo(cyl)))
  expect_hybrid(mtcars, sum(!!quo(cyl)))
  expect_hybrid(mtcars, sd(!!quo(cyl)))
  expect_hybrid(mtcars, var(!!quo(cyl)))

  expect_hybrid(mtcars, min(!!quo(cyl)))
  expect_hybrid(mtcars, max(!!quo(cyl)))

  expect_hybrid(mtcars, lead(!!quo(cyl)))
  expect_hybrid(mtcars, lag(!!quo(cyl)))
})

test_that("window handlers supports quosured symbols", {
  expect_hybrid(mtcars, ntile(!!quo(disp), n = 2))
  expect_hybrid(mtcars, min_rank(!!quo(disp)))
  expect_hybrid(mtcars, percent_rank(!!quo(disp)))
  expect_hybrid(mtcars, dense_rank(!!quo(disp)))
  expect_hybrid(mtcars, dense_rank(!!quo(disp)))
})

test_that("n_distinct() handler supports quosured symbols", {
  expect_hybrid(mtcars, n_distinct(!!quo(cyl)))
})

test_that("nth(), first() and last() support quosured symbols", {
  expect_hybrid(mtcars, first(!!quo(cyl)))
  expect_hybrid(mtcars, last(!!quo(cyl)))
  expect_hybrid(mtcars, nth(!!quo(cyl), n = 2))
})

# test_that("hybrid evaluation can be disabled locally (#3255)", {
#   tbl <- data.frame(x = 1:10)
#
#   first <- function(...) 42
#   expect_equal(summarise(tbl, y = first(x))$y, 42)
#   expect_equal(summarise(tbl, y = dplyr::first(x))$y, 1)
#
#   last <- function(...) 42
#   expect_equal(summarise(tbl, y = last(x))$y, 42)
#   expect_equal(summarise(tbl, y = dplyr::last(x))$y, 10)
#
#   nth <- function(...) 42
#   expect_equal(summarise(tbl, y = nth(x, 2L))$y, 42)
#   expect_equal(summarise(tbl, y = dplyr::nth(x, 2))$y, 2)
#
#   mean <- function(...) 42
#   tbl <- data.frame(x = 1:10)
#   expect_equal(summarise(tbl, y = mean(x))$y, 42)
#   expect_equal(summarise(tbl, y = base::mean(x))$y, 5.5)
#
#   var <- function(...) 42
#   expect_equal(summarise(tbl, y = var(x))$y, 42)
#   expect_equal(summarise(tbl, y = stats::var(x))$y, stats::var(tbl$x))
#
#   sd <- function(...) 42
#   expect_equal(summarise(tbl, y = sd(x))$y, 42)
#   expect_equal(summarise(tbl, y = stats::sd(x))$y, stats::sd(tbl$x))
#
#   row_number <- function() 42
#   expect_equal(mutate(tbl, y = row_number())$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::row_number())$y, 1:10)
#
#   ntile <- function(x, n) 42
#   expect_equal(mutate(tbl, y = ntile(x, 2))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::ntile(x, 2))$y, rep(1:2, each = 5))
#
#   min_rank <- function(x) 42
#   expect_equal(mutate(tbl, y = min_rank(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::min_rank(x))$y, 1:10)
#
#   percent_rank <- function(x) 42
#   expect_equal(mutate(tbl, y = percent_rank(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::percent_rank(x))$y, dplyr::percent_rank(1:10))
#
#   dense_rank <- function(x) 42
#   expect_equal(mutate(tbl, y = dense_rank(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::dense_rank(x))$y, dplyr::dense_rank(1:10))
#
#   cume_dist <- function(x) 42
#   expect_equal(mutate(tbl, y = cume_dist(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::cume_dist(x))$y, dplyr::cume_dist(1:10))
#
#   lead <- function(x) 42
#   expect_equal(mutate(tbl, y = lead(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::lead(x))$y, dplyr::lead(1:10))
#
#   lag <- function(x) 42
#   expect_equal(mutate(tbl, y = lag(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::lag(x))$y, dplyr::lag(1:10))
#
#   `%in%` <- function(x, y) TRUE
#   expect_identical(filter(tbl, x %in% 3), tbl)
#
#   min <- function(x) 42
#   expect_equal(summarise(tbl, y = min(x))$y, 42)
#   expect_equal(summarise(tbl, y = base::min(x))$y, 1L)
#
#   max <- function(x) 42
#   expect_equal(summarise(tbl, y = max(x))$y, 42)
#   expect_equal(summarise(tbl, y = base::max(x))$y, 10L)
#
#   n <- function() 42
#   expect_equal(summarise(tbl, y = n())$y, 42)
#   expect_equal(summarise(tbl, y = dplyr::n())$y, 10L)
#
#   n_distinct <- function(x) 42
#   expect_equal(summarise(tbl, y = n_distinct(x))$y, 42)
#   expect_equal(summarise(tbl, y = dplyr::n_distinct(x))$y, 10L)
# })
#
# test_that("hybrid first and last fall back to R eval when no argument (#3589)", {
#   res <- mutate(tibble(v1 = 5:6), v2 = 1:4 %>% first(), v3 = 1:4 %>% last())
#   expect_equal(res$v2, rep(1L, 2))
#   expect_equal(res$v3, rep(4L, 2))
# })
