context("hybrid")

test_that("hybrid evaluation environment is cleaned up (#2358)", {
  get_data_mask_active_env <- function(e){
    env_parent(env_parent(e))
  }

  # Can't use pipe here, f and g should have top-level parent.env()
  df <- tibble(a = 1) %>% group_by(a)
  df <- mutate(df, f = {
    a
    list(function() {})
  })
  df <- mutate(df, g = {
    f
    list(quo(.))
  })
  df <- mutate(df, h = {
    g
    list(~ .)
  })
  df <- mutate(df, i = {
    h
    list(.data)
  })

  expect_warning(
    expect_null(get_data_mask_active_env(environment(df$f[[1]]))[["a"]]),
    "Hybrid callback proxy out of scope",
    fixed = TRUE
  )
  expect_warning(
    expect_null(get_data_mask_active_env(environment(df$g[[1]]))[["g"]]),
    "Hybrid callback proxy out of scope",
    fixed = TRUE
  )
  expect_warning(
    expect_null(get_data_mask_active_env(environment(df$h[[1]]))[["g"]]),
    "Hybrid callback proxy out of scope",
    fixed = TRUE
  )
  expect_warning(
    expect_null(df$i[[1]][["h"]]),
    "Hybrid callback proxy out of scope",
    fixed = TRUE
  )
})

test_that("n() uses hybrid evaluation", {
  d <- tibble(a = 1:5)
  expect_hybrid(d, n())
  expect_hybrid(d, dplyr::n())
  expect_hybrid(d, (!!n)())
  expect_not_hybrid(d, list(1:n()))
  expect_not_hybrid(d, n() + 1)
})

test_that("<column> %in% <column> is hybrid", {
  d <- tibble(a = rep(1L, 3), b = 1:3)

  expect_hybrid(d, a %in% b)
  expect_hybrid(d, (!!`%in%`)(a, b))

  expect_not_hybrid(d, a %in% b[1])
  expect_not_hybrid(d, a[1] %in% b)
  expect_not_hybrid(d, a %in% 1:3)
})

test_that("min() and max() are hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))
  expect_hybrid(d, min(int))
  expect_hybrid(d, min(dbl))
  expect_hybrid(d, (!!min)(int))
  expect_hybrid(d, (!!min)(dbl))
  expect_hybrid(d, base::min(int))
  expect_hybrid(d, base::min(dbl))
  expect_not_hybrid(d, min(chr))

  expect_hybrid(d, min(int, na.rm = TRUE))
  expect_hybrid(d, min(dbl, na.rm = TRUE))
  expect_hybrid(d, (!!min)(int, na.rm = TRUE))
  expect_hybrid(d, (!!min)(dbl, na.rm = TRUE))
  expect_hybrid(d, base::min(int, na.rm = TRUE))
  expect_hybrid(d, base::min(dbl, na.rm = TRUE))
  expect_not_hybrid(d, min(int, na.rm = pi == pi))
  expect_not_hybrid(d, min(dbl, na.rm = pi == pi))
  expect_not_hybrid(d, min(dbl, na.rm = F))
  expect_not_hybrid(d, min(dbl, na.rm = T))
  expect_not_hybrid(d, min(chr, na.rm = TRUE))

  expect_hybrid(d, min(int, na.rm = FALSE))
  expect_hybrid(d, min(dbl, na.rm = FALSE))
  expect_hybrid(d, (!!min)(int, na.rm = FALSE))
  expect_hybrid(d, (!!min)(dbl, na.rm = FALSE))
  expect_hybrid(d, base::min(int, na.rm = FALSE))
  expect_hybrid(d, base::min(dbl, na.rm = FALSE))
  expect_not_hybrid(d, min(chr, na.rm = FALSE))

  expect_hybrid(d, max(int))
  expect_hybrid(d, max(dbl))
  expect_hybrid(d, (!!max)(int))
  expect_hybrid(d, (!!max)(dbl))
  expect_hybrid(d, base::max(int))
  expect_hybrid(d, base::max(dbl))
  expect_not_hybrid(d, max(chr))

  expect_hybrid(d, max(int, na.rm = TRUE))
  expect_hybrid(d, max(dbl, na.rm = TRUE))
  expect_hybrid(d, (!!max)(int, na.rm = TRUE))
  expect_hybrid(d, (!!max)(dbl, na.rm = TRUE))
  expect_hybrid(d, base::max(int, na.rm = TRUE))
  expect_hybrid(d, base::max(dbl, na.rm = TRUE))
  expect_not_hybrid(d, max(int, na.rm = pi == pi))
  expect_not_hybrid(d, max(dbl, na.rm = pi == pi))
  expect_not_hybrid(d, max(dbl, na.rm = F))
  expect_not_hybrid(d, max(dbl, na.rm = T))
  expect_not_hybrid(d, max(chr, na.rm = TRUE))

  expect_hybrid(d, max(int, na.rm = FALSE))
  expect_hybrid(d, max(dbl, na.rm = FALSE))
  expect_hybrid(d, (!!max)(int, na.rm = FALSE))
  expect_hybrid(d, (!!max)(dbl, na.rm = FALSE))
  expect_hybrid(d, base::max(int, na.rm = FALSE))
  expect_hybrid(d, base::max(dbl, na.rm = FALSE))
  expect_not_hybrid(d, max(chr, na.rm = FALSE))
})

test_that("first() and last() are hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, first(int))
  expect_hybrid(d, first(dbl))
  expect_hybrid(d, first(chr))
  expect_hybrid(d, (!!first)(int))
  expect_hybrid(d, (!!first)(dbl))
  expect_hybrid(d, (!!first)(chr))
  expect_hybrid(d, dplyr::first(int))
  expect_hybrid(d, dplyr::first(dbl))
  expect_hybrid(d, dplyr::first(chr))

  expect_hybrid(d, first(int, default = 1L))
  expect_hybrid(d, first(dbl, default = 2))
  expect_hybrid(d, first(chr, default = ""))
  expect_hybrid(d, (!!first)(int, default = 1L))
  expect_hybrid(d, (!!first)(dbl, default = 2))
  expect_hybrid(d, (!!first)(chr, default = ""))
  expect_hybrid(d, dplyr::first(int, default = 1L))
  expect_hybrid(d, dplyr::first(dbl, default = 2))
  expect_hybrid(d, dplyr::first(chr, default = ""))

  expect_hybrid(d, last(int))
  expect_hybrid(d, last(dbl))
  expect_hybrid(d, last(chr))
  expect_hybrid(d, (!!last)(int))
  expect_hybrid(d, (!!last)(dbl))
  expect_hybrid(d, (!!last)(chr))
  expect_hybrid(d, dplyr::last(int))
  expect_hybrid(d, dplyr::last(dbl))
  expect_hybrid(d, dplyr::last(chr))

  expect_hybrid(d, last(int, default = 1L))
  expect_hybrid(d, last(dbl, default = 2))
  expect_hybrid(d, last(chr, default = ""))
  expect_hybrid(d, (!!first)(int, default = 1L))
  expect_hybrid(d, (!!first)(dbl, default = 2))
  expect_hybrid(d, (!!first)(chr, default = ""))
  expect_hybrid(d, dplyr::last(int, default = 1L))
  expect_hybrid(d, dplyr::last(dbl, default = 2))
  expect_hybrid(d, dplyr::last(chr, default = ""))

  expect_not_hybrid(d, int %>% first())
  expect_not_hybrid(d, int %>% last())
})

test_that("nth(<column>, n = <int-ish>) is hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, nth(int, n = 1))
  expect_hybrid(d, nth(int, n = 1L))
  expect_hybrid(d, nth(int, n = -1))
  expect_hybrid(d, nth(int, n = -1L))
  expect_hybrid(d, (!!nth)(int, n = 1))
  expect_hybrid(d, (!!nth)(int, n = 1L))
  expect_hybrid(d, (!!nth)(int, n = -1))
  expect_hybrid(d, (!!nth)(int, n = -1L))
  expect_not_hybrid(d, nth(dbl, n = 2^40))
  expect_not_hybrid(d, nth(int, n = NA))
  expect_hybrid(d, dplyr::nth(int, n = 1))
  expect_hybrid(d, dplyr::nth(int, n = 1L))
  expect_hybrid(d, dplyr::nth(int, n = -1))
  expect_hybrid(d, dplyr::nth(int, n = -1L))
  expect_not_hybrid(d, dplyr::nth(int, n = NA))

  expect_hybrid(d, nth(dbl, n = 1))
  expect_hybrid(d, nth(dbl, n = 1L))
  expect_hybrid(d, nth(dbl, n = -1))
  expect_hybrid(d, nth(dbl, n = -1L))
  expect_hybrid(d, (!!nth)(dbl, n = 1))
  expect_hybrid(d, (!!nth)(dbl, n = 1L))
  expect_hybrid(d, (!!nth)(dbl, n = -1))
  expect_hybrid(d, (!!nth)(dbl, n = -1L))
  expect_not_hybrid(d, nth(dbl, n = NA))
  expect_hybrid(d, dplyr::nth(dbl, n = 1))
  expect_hybrid(d, dplyr::nth(dbl, n = 1L))
  expect_hybrid(d, dplyr::nth(dbl, n = -1))
  expect_hybrid(d, dplyr::nth(dbl, n = -1L))
  expect_not_hybrid(d, dplyr::nth(dbl, n = NA))

  expect_hybrid(d, nth(chr, n = 1))
  expect_hybrid(d, nth(chr, n = 1L))
  expect_hybrid(d, nth(chr, n = -1))
  expect_hybrid(d, nth(chr, n = -1L))
  expect_hybrid(d, (!!nth)(chr, n = 1))
  expect_hybrid(d, (!!nth)(chr, n = 1L))
  expect_hybrid(d, (!!nth)(chr, n = -1))
  expect_hybrid(d, (!!nth)(chr, n = -1L))
  expect_not_hybrid(d, nth(chr, n = NA))
  expect_hybrid(d, dplyr::nth(chr, n = 1))
  expect_hybrid(d, dplyr::nth(chr, n = 1L))
  expect_hybrid(d, dplyr::nth(chr, n = -1))
  expect_hybrid(d, dplyr::nth(chr, n = -1L))
  expect_not_hybrid(d, nth(chr, n = NA))
})

test_that("hybrid nth() handles negative n (#3821)", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))
  res <- summarise(d,
    int = nth(int, -1),
    dbl = nth(dbl, -1),
    chr = nth(chr, -1)
  )
  expect_equal(res, summarise_all(d, nth, 2))
})

test_that("nth(<column>, n = <int-ish>, default = <scalar>) is hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, nth(int, n = 1, default = 1L))
  expect_hybrid(d, nth(int, n = 1L, default = 1L))
  expect_hybrid(d, nth(int, n = -1, default = 1L))
  expect_hybrid(d, nth(int, n = -1L, default = 1L))
  expect_hybrid(d, (!!nth)(int, n = 1, default = 1L))
  expect_hybrid(d, (!!nth)(int, n = 1L, default = 1L))
  expect_hybrid(d, (!!nth)(int, n = -1, default = 1L))
  expect_hybrid(d, (!!nth)(int, n = -1L, default = 1L))
  expect_hybrid(d, dplyr::nth(int, n = 1, default = 1L))
  expect_hybrid(d, dplyr::nth(int, n = 1L, default = 1L))
  expect_hybrid(d, dplyr::nth(int, n = -1, default = 1L))
  expect_hybrid(d, dplyr::nth(int, n = -1L, default = 1L))

  expect_hybrid(d, nth(dbl, n = 1, default = 1))
  expect_hybrid(d, nth(dbl, n = 1L, default = 1))
  expect_hybrid(d, nth(dbl, n = -1, default = 1))
  expect_hybrid(d, nth(dbl, n = -1L, default = 1))
  expect_hybrid(d, (!!nth)(dbl, n = 1, default = 1))
  expect_hybrid(d, (!!nth)(dbl, n = 1L, default = 1))
  expect_hybrid(d, (!!nth)(dbl, n = -1, default = 1))
  expect_hybrid(d, (!!nth)(dbl, n = -1L, default = 1))
  expect_hybrid(d, dplyr::nth(dbl, n = 1, default = 1))
  expect_hybrid(d, dplyr::nth(dbl, n = 1L, default = 1))
  expect_hybrid(d, dplyr::nth(dbl, n = -1, default = 1))
  expect_hybrid(d, dplyr::nth(dbl, n = -1L, default = 1))

  expect_hybrid(d, nth(chr, n = 1, default = ""))
  expect_hybrid(d, nth(chr, n = 1L, default = ""))
  expect_hybrid(d, nth(chr, n = -1, default = ""))
  expect_hybrid(d, nth(chr, n = -1L, default = ""))
  expect_hybrid(d, (!!nth)(chr, n = 1, default = ""))
  expect_hybrid(d, (!!nth)(chr, n = 1L, default = ""))
  expect_hybrid(d, (!!nth)(chr, n = -1, default = ""))
  expect_hybrid(d, (!!nth)(chr, n = -1L, default = ""))
  expect_hybrid(d, dplyr::nth(chr, n = 1, default = ""))
  expect_hybrid(d, dplyr::nth(chr, n = 1L, default = ""))
  expect_hybrid(d, dplyr::nth(chr, n = -1, default = ""))
  expect_hybrid(d, dplyr::nth(chr, n = -1L, default = ""))
})

test_that("Expression folds unary minus when looking for constant ints", {
  b <- -3L
  data <- tibble(a = 1:5)

  expect_hybrid(data, nth(a, n = -3L))
  expect_hybrid(data, nth(a, n = b))
  expect_hybrid(data, nth(a, n = -b))
  expect_hybrid(data, nth(a, n = !!b))
})

test_that("lead() and lag() are hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, lead(int))
  expect_hybrid(d, lead(dbl))
  expect_hybrid(d, lead(chr))
  expect_hybrid(d, (!!lead)(int))
  expect_hybrid(d, (!!lead)(dbl))
  expect_hybrid(d, (!!lead)(chr))
  expect_hybrid(d, dplyr::lead(int))
  expect_hybrid(d, dplyr::lead(dbl))
  expect_hybrid(d, dplyr::lead(chr))

  expect_hybrid(d, lead(int, n = 1))
  expect_hybrid(d, lead(dbl, n = 1))
  expect_hybrid(d, lead(chr, n = 1))
  expect_hybrid(d, (!!lead)(int, n = 1))
  expect_hybrid(d, (!!lead)(dbl, n = 1))
  expect_hybrid(d, (!!lead)(chr, n = 1))
  expect_hybrid(d, dplyr::lead(int, n = 1))
  expect_hybrid(d, dplyr::lead(dbl, n = 1))
  expect_hybrid(d, dplyr::lead(chr, n = 1))

  expect_hybrid(d, lead(int, n = 1L))
  expect_hybrid(d, lead(dbl, n = 1L))
  expect_hybrid(d, lead(chr, n = 1L))
  expect_hybrid(d, (!!lead)(int, n = 1L))
  expect_hybrid(d, (!!lead)(dbl, n = 1L))
  expect_hybrid(d, (!!lead)(chr, n = 1L))
  expect_hybrid(d, dplyr::lead(int, n = 1L))
  expect_hybrid(d, dplyr::lead(dbl, n = 1L))
  expect_hybrid(d, dplyr::lead(chr, n = 1L))
})

test_that("lead() and lag() are not hybrid with negative `n`", {
  d <- tibble(int = 1:2)
  minus1 <- -1L
  expect_not_hybrid(d, lead(int, !!minus1))
  expect_not_hybrid(d, lag(int, !!minus1))
})

test_that("lead() and lag() are echo with n == 0", {
  d <- tibble(int = 1:2)
  expect_equal(attr(hybrid_call(d, lead(int, n = 0L)), "cpp_class"), "echo")
  expect_equal(attr(hybrid_call(d, lag(int, n = 0L)), "cpp_class"), "echo")
})

test_that("sum is hybrid", {
  d <- tibble(lgl = c(TRUE, FALSE), int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, sum(lgl))
  expect_hybrid(d, sum(int))
  expect_hybrid(d, sum(dbl))
  expect_hybrid(d, (!!sum)(lgl))
  expect_hybrid(d, (!!sum)(int))
  expect_hybrid(d, (!!sum)(dbl))
  expect_hybrid(d, base::sum(lgl))
  expect_hybrid(d, base::sum(int))
  expect_hybrid(d, base::sum(dbl))
  expect_not_hybrid(d, sum(chr))

  expect_hybrid(d, sum(lgl, na.rm = TRUE))
  expect_hybrid(d, sum(int, na.rm = TRUE))
  expect_hybrid(d, sum(dbl, na.rm = TRUE))
  expect_hybrid(d, (!!sum)(lgl, na.rm = TRUE))
  expect_hybrid(d, (!!sum)(int, na.rm = TRUE))
  expect_hybrid(d, (!!sum)(dbl, na.rm = TRUE))
  expect_hybrid(d, base::sum(lgl, na.rm = TRUE))
  expect_hybrid(d, base::sum(int, na.rm = TRUE))
  expect_hybrid(d, base::sum(dbl, na.rm = TRUE))
  expect_not_hybrid(d, sum(chr, na.rm = TRUE))

  expect_hybrid(d, sum(lgl, na.rm = FALSE))
  expect_hybrid(d, sum(int, na.rm = FALSE))
  expect_hybrid(d, sum(dbl, na.rm = FALSE))
  expect_hybrid(d, (!!sum)(lgl, na.rm = FALSE))
  expect_hybrid(d, (!!sum)(int, na.rm = FALSE))
  expect_hybrid(d, (!!sum)(dbl, na.rm = FALSE))
  expect_hybrid(d, base::sum(lgl, na.rm = FALSE))
  expect_hybrid(d, base::sum(int, na.rm = FALSE))
  expect_hybrid(d, base::sum(dbl, na.rm = FALSE))
  expect_not_hybrid(d, sum(chr, na.rm = FALSE))
})

test_that("mean is hybrid", {
  d <- tibble(lgl = c(TRUE, FALSE), int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, mean(lgl))
  expect_hybrid(d, mean(int))
  expect_hybrid(d, mean(dbl))
  expect_hybrid(d, (!!mean)(lgl))
  expect_hybrid(d, (!!mean)(int))
  expect_hybrid(d, (!!mean)(dbl))
  expect_hybrid(d, base::mean(lgl))
  expect_hybrid(d, base::mean(int))
  expect_hybrid(d, base::mean(dbl))
  expect_not_hybrid(d, mean(chr))

  expect_hybrid(d, mean(lgl, na.rm = TRUE))
  expect_hybrid(d, mean(int, na.rm = TRUE))
  expect_hybrid(d, mean(dbl, na.rm = TRUE))
  expect_hybrid(d, (!!mean)(lgl, na.rm = TRUE))
  expect_hybrid(d, (!!mean)(int, na.rm = TRUE))
  expect_hybrid(d, (!!mean)(dbl, na.rm = TRUE))
  expect_hybrid(d, base::mean(lgl, na.rm = TRUE))
  expect_hybrid(d, base::mean(int, na.rm = TRUE))
  expect_hybrid(d, base::mean(dbl, na.rm = TRUE))
  expect_not_hybrid(d, mean(chr, na.rm = TRUE))

  expect_hybrid(d, mean(lgl, na.rm = FALSE))
  expect_hybrid(d, mean(int, na.rm = FALSE))
  expect_hybrid(d, mean(dbl, na.rm = FALSE))
  expect_hybrid(d, (!!mean)(lgl, na.rm = FALSE))
  expect_hybrid(d, (!!mean)(int, na.rm = FALSE))
  expect_hybrid(d, (!!mean)(dbl, na.rm = FALSE))
  expect_hybrid(d, base::mean(lgl, na.rm = FALSE))
  expect_hybrid(d, base::mean(int, na.rm = FALSE))
  expect_hybrid(d, base::mean(dbl, na.rm = FALSE))
  expect_not_hybrid(d, mean(chr, na.rm = FALSE))
})

test_that("sd is hybrid", {
  d <- tibble(lgl = c(TRUE, FALSE), int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, sd(lgl))
  expect_hybrid(d, sd(int))
  expect_hybrid(d, sd(dbl))
  expect_hybrid(d, (!!sd)(lgl))
  expect_hybrid(d, (!!sd)(int))
  expect_hybrid(d, (!!sd)(dbl))
  expect_hybrid(d, stats::sd(lgl))
  expect_hybrid(d, stats::sd(int))
  expect_hybrid(d, stats::sd(dbl))
  expect_not_hybrid(d, sd(chr))

  expect_hybrid(d, sd(lgl, na.rm = TRUE))
  expect_hybrid(d, sd(int, na.rm = TRUE))
  expect_hybrid(d, sd(dbl, na.rm = TRUE))
  expect_hybrid(d, (!!sd)(lgl, na.rm = TRUE))
  expect_hybrid(d, (!!sd)(int, na.rm = TRUE))
  expect_hybrid(d, (!!sd)(dbl, na.rm = TRUE))
  expect_hybrid(d, stats::sd(lgl, na.rm = TRUE))
  expect_hybrid(d, stats::sd(int, na.rm = TRUE))
  expect_hybrid(d, stats::sd(dbl, na.rm = TRUE))
  expect_not_hybrid(d, sd(chr, na.rm = TRUE))

  expect_hybrid(d, sd(lgl, na.rm = FALSE))
  expect_hybrid(d, sd(int, na.rm = FALSE))
  expect_hybrid(d, sd(dbl, na.rm = FALSE))
  expect_hybrid(d, (!!sd)(lgl, na.rm = FALSE))
  expect_hybrid(d, (!!sd)(int, na.rm = FALSE))
  expect_hybrid(d, (!!sd)(dbl, na.rm = FALSE))
  expect_hybrid(d, stats::sd(lgl, na.rm = FALSE))
  expect_hybrid(d, stats::sd(int, na.rm = FALSE))
  expect_hybrid(d, stats::sd(dbl, na.rm = FALSE))
  expect_not_hybrid(d, sd(chr, na.rm = FALSE))
})

test_that("var is hybrid", {
  d <- tibble(lgl = c(TRUE, FALSE), int = 1:2, dbl = c(1,2), chr = c("a", "b"))

  expect_hybrid(d, var(lgl))
  expect_hybrid(d, var(int))
  expect_hybrid(d, var(dbl))
  expect_hybrid(d, (!!var)(lgl))
  expect_hybrid(d, (!!var)(int))
  expect_hybrid(d, (!!var)(dbl))
  expect_hybrid(d, stats::var(lgl))
  expect_hybrid(d, stats::var(int))
  expect_hybrid(d, stats::var(dbl))
  expect_not_hybrid(d, var(chr))

  expect_hybrid(d, var(lgl, na.rm = TRUE))
  expect_hybrid(d, var(int, na.rm = TRUE))
  expect_hybrid(d, var(dbl, na.rm = TRUE))
  expect_hybrid(d, (!!var)(lgl, na.rm = TRUE))
  expect_hybrid(d, (!!var)(int, na.rm = TRUE))
  expect_hybrid(d, (!!var)(dbl, na.rm = TRUE))
  expect_hybrid(d, stats::var(lgl, na.rm = TRUE))
  expect_hybrid(d, stats::var(int, na.rm = TRUE))
  expect_hybrid(d, stats::var(dbl, na.rm = TRUE))
  expect_not_hybrid(d, var(chr, na.rm = TRUE))

  expect_hybrid(d, var(lgl, na.rm = FALSE))
  expect_hybrid(d, var(int, na.rm = FALSE))
  expect_hybrid(d, var(dbl, na.rm = FALSE))
  expect_hybrid(d, (!!var)(lgl, na.rm = FALSE))
  expect_hybrid(d, (!!var)(int, na.rm = FALSE))
  expect_hybrid(d, (!!var)(dbl, na.rm = FALSE))
  expect_hybrid(d, stats::var(lgl, na.rm = FALSE))
  expect_hybrid(d, stats::var(int, na.rm = FALSE))
  expect_hybrid(d, stats::var(dbl, na.rm = FALSE))
  expect_not_hybrid(d, var(chr, na.rm = FALSE))
})

test_that("row_number() is hybrid", {
  d <- tibble(a = 1:5)
  expect_hybrid(d, row_number())
  expect_hybrid(d, (!!row_number)())
  expect_hybrid(d, dplyr::row_number())
})

test_that("ntile() is hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2))
  expect_hybrid(d, ntile(n = 2L))
  expect_hybrid(d, ntile(n = 2))
  expect_hybrid(d, (!!ntile)(n = 2L))
  expect_hybrid(d, (!!ntile)(n = 2))
  expect_hybrid(d, dplyr::ntile(n = 2L))
  expect_hybrid(d, dplyr::ntile(n = 2))
  expect_not_hybrid(d, ntile(n = NA_integer_))
  expect_not_hybrid(d, ntile(n = NA_real_))
  expect_not_hybrid(d, ntile(n = NA))

  expect_hybrid(d, ntile(int, n = 2L))
  expect_hybrid(d, ntile(int, n = 2))
  expect_hybrid(d, (!!ntile)(int, n = 2L))
  expect_hybrid(d, (!!ntile)(int, n = 2))
  expect_hybrid(d, dplyr::ntile(int, n = 2L))
  expect_hybrid(d, dplyr::ntile(int, n = 2))
  expect_not_hybrid(d, ntile(int, n = NA_integer_))
  expect_not_hybrid(d, ntile(int, n = NA_real_))
  expect_not_hybrid(d, ntile(int, n = NA))

  expect_hybrid(d, ntile(dbl, n = 2L))
  expect_hybrid(d, ntile(dbl, n = 2))
  expect_hybrid(d, (!!ntile)(dbl, n = 2L))
  expect_hybrid(d, (!!ntile)(dbl, n = 2))
  expect_hybrid(d, dplyr::ntile(dbl, n = 2L))
  expect_hybrid(d, dplyr::ntile(dbl, n = 2))
  expect_not_hybrid(d, ntile(dbl, n = NA_integer_))
  expect_not_hybrid(d, ntile(dbl, n = NA_real_))
  expect_not_hybrid(d, ntile(dbl, n = NA))
})

test_that("min_rank(), percent_rank(), dense_rank(), cume_dist() are hybrid", {
  d <- tibble(int = 1:2, dbl = c(1,2))

  expect_hybrid(d, min_rank(int))
  expect_hybrid(d, min_rank(dbl))
  expect_hybrid(d, (!!min_rank)(int))
  expect_hybrid(d, (!!min_rank)(dbl))
  expect_hybrid(d, dplyr::min_rank(int))
  expect_hybrid(d, dplyr::min_rank(dbl))

  expect_hybrid(d, percent_rank(int))
  expect_hybrid(d, percent_rank(dbl))
  expect_hybrid(d, (!!percent_rank)(int))
  expect_hybrid(d, (!!percent_rank)(dbl))
  expect_hybrid(d, dplyr::percent_rank(int))
  expect_hybrid(d, dplyr::percent_rank(dbl))

  expect_hybrid(d, dense_rank(int))
  expect_hybrid(d, dense_rank(dbl))
  expect_hybrid(d, (!!dense_rank)(int))
  expect_hybrid(d, (!!dense_rank)(dbl))
  expect_hybrid(d, dplyr::dense_rank(int))
  expect_hybrid(d, dplyr::dense_rank(dbl))

  expect_hybrid(d, cume_dist(int))
  expect_hybrid(d, cume_dist(dbl))
  expect_hybrid(d, (!!cume_dist)(int))
  expect_hybrid(d, (!!cume_dist)(dbl))
  expect_hybrid(d, dplyr::cume_dist(int))
  expect_hybrid(d, dplyr::cume_dist(dbl))
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

test_that("nth(), first() and last() support quosured symbols", {
  expect_hybrid(mtcars, first(!!quo(cyl)))
  expect_hybrid(mtcars, last(!!quo(cyl)))
  expect_hybrid(mtcars, nth(!!quo(cyl), n = 2))
  expect_not_hybrid(mtcars, nth(!!quo(cyl), n = NA))
})

test_that("hybrid evaluation can be disabled locally (#3255)", {
  tbl <- data.frame(x = 1:10)

  first <- function(...) 42
  expect_not_hybrid(tbl, first(x))
  expect_hybrid(tbl, dplyr::first(x))

  last <- function(...) 42
  expect_not_hybrid(tbl, last(x))
  expect_hybrid(tbl, dplyr::last(x))

  nth <- function(...) 42
  expect_not_hybrid(tbl, nth(x, n = 2L))
  expect_hybrid(tbl, dplyr::nth(x, n = 2L))

  mean <- function(...) 42
  tbl <- data.frame(x = 1:10)
  expect_not_hybrid(tbl, mean(x))
  expect_hybrid(tbl, base::mean(x))

  var <- function(...) 42
  expect_not_hybrid(tbl, var(x))
  expect_hybrid(tbl, stats::var(x))

  sd <- function(...) 42
  expect_not_hybrid(tbl, sd(x))
  expect_hybrid(tbl, stats::sd(x))

  row_number <- function() 42
  expect_not_hybrid(tbl, row_number(x))
  expect_hybrid(tbl, dplyr::row_number(x))

  ntile <- function(x, n) 42
  expect_not_hybrid(tbl, ntile(x, n = 2))
  expect_hybrid(tbl, dplyr::ntile(x, n = 2))

  min_rank <- function(x) 42
  expect_not_hybrid(tbl, min_rank(x))
  expect_hybrid(tbl, dplyr::min_rank(x))

  percent_rank <- function(x) 42
  expect_not_hybrid(tbl, percent_rank(x))
  expect_hybrid(tbl, dplyr::percent_rank(x))

  dense_rank <- function(x) 42
  expect_not_hybrid(tbl, dense_rank(x))
  expect_hybrid(tbl, dplyr::dense_rank(x))

  cume_dist <- function(x) 42
  expect_not_hybrid(tbl, cume_dist(x))
  expect_hybrid(tbl, dplyr::cume_dist(x))

  lead <- function(x) 42
  expect_not_hybrid(tbl, lead(x))
  expect_hybrid(tbl, dplyr::lead(x))

  lag <- function(x) 42
  expect_not_hybrid(tbl, lag(x))
  expect_hybrid(tbl, dplyr::lag(x))

  `%in%` <- function(x, y) TRUE
  expect_not_hybrid(tbl, x %in% 3)

  min <- function(x) 42
  expect_not_hybrid(tbl, min(x))
  expect_hybrid(tbl, base::min(x))

  max <- function(x) 42
  expect_not_hybrid(tbl, max(x))
  expect_hybrid(tbl, base::max(x))

  n <- function() 42
  expect_not_hybrid(tbl, n())
  expect_hybrid(tbl, dplyr::n())

})

test_that("verbs can nest with well defined behavior (#2080)", {
  df <- tibble(x = list(
    tibble(y = 1:2),
    tibble(y = 1:3),
    tibble(y = 1:4)
  ))

  nrows <- function(df) {
    df %>% summarise(n = n()) %>% .[["n"]]
  }

  nrows_magrittr_lambda <- . %>% summarise(n = n()) %>% .[["n"]]

  res <- mutate( df,
    n1 = x %>% map_int(nrows),
    n2 = x %>% map_int(. %>% summarise(n = n()) %>% .[["n"]]),
    n4 = map_int(x, function(df) summarise(df, n = n())[["n"]]),
    n5 = map_int(x, nrows_magrittr_lambda)
  )
  expect_equal(res$n1, res$n2)
  expect_equal(res$n1, res$n4)
  expect_equal(res$n1, res$n5)
})

test_that("hybrid first, last and nth operate within groups (#3868)", {
  first_ <- function(x) x[1]
  last_  <- function(x) tail(x, 1L)
  nth_   <- function(x, n) x[n]
  expect_identical(
    iris %>% group_by(Species) %>% summarise(Sepal.Length = first(Sepal.Length)),
    iris %>% group_by(Species) %>% summarise(Sepal.Length = first_(Sepal.Length))
  )
  expect_identical(
    iris %>% group_by(Species) %>% summarise(Sepal.Length = last(Sepal.Length)),
    iris %>% group_by(Species) %>% summarise(Sepal.Length = last_(Sepal.Length))
  )
  expect_identical(
    iris %>% group_by(Species) %>% summarise(Sepal.Length = nth(Sepal.Length, n = 2L)),
    iris %>% group_by(Species) %>% summarise(Sepal.Length = nth_(Sepal.Length, n = 2L))
  )
})

test_that("hybrid resolves the symbol", {
  mean <- sum
  out <- summarise(data.frame(x = 1:10), mean(x))
  expect_equal(out[[1]], sum(1:10))

  call <- hybrid_call(data.frame(x = 1:10), mean(x))
  expect_true(call)
  expect_equal(attr(call, "fun"), "sum")
  expect_equal(attr(call, "package"), "base")
})
