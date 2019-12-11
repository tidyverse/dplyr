library(profvis)
library(dplyr)
library(vctrs)
library(bench)

bench_mean <- function(n = 1e6, ngroups = 1000) {
  args <- list(n = n, ngroups = ngroups)

  # master, internal_mean
  master_length <- callr::r(function(n, ngroups) {
    library(dplyr, warn.conflicts = FALSE)
    library(purrr)
    library(vctrs)

    df <- tibble(x = rnorm(n), g = sample(rep(1:ngroups, n / ngroups))) %>% group_by(g)
    bench::mark(summarise(df, a = length(x))) %>%
      mutate(branch = "master", fun = "length", n = n, ngroups = ngroups) %>%
      select(branch, fun, n, ngroups, min:last_col())
  }, args = args)

  master_internal <- callr::r(function(n, ngroups) {
    library(dplyr, warn.conflicts = FALSE)
    library(purrr)
    library(vctrs)

    df <- tibble(x = rnorm(n), g = sample(rep(1:ngroups, n / ngroups))) %>% group_by(g)
    bench::mark(summarise(df, a = .Internal(mean(x)))) %>%
      mutate(branch = "master", fun = ".Internal(mean(.))", n = n, ngroups = ngroups) %>%
      select(branch, fun, n, ngroups, min:last_col())
  }, args = args)

  master_mean <- callr::r(function(n, ngroups) {
    library(dplyr, warn.conflicts = FALSE)
    library(purrr)
    library(vctrs)

    df <- tibble(x = rnorm(n), g = sample(rep(1:ngroups, n / ngroups))) %>% group_by(g)
    bench::mark(summarise(df, a = mean(x))) %>%
      mutate(branch = "master", fun = "mean(.)", n = n, ngroups = ngroups) %>%
      select(branch, fun, n, ngroups, min:last_col())
  }, args = args)

  released_internal <- callr::r(function(n, ngroups) {
    library(dplyr, warn.conflicts = FALSE)
    library(purrr)
    library(vctrs)

    df <- tibble(x = rnorm(n), g = sample(rep(1:ngroups, n / ngroups))) %>% group_by(g)
    bench::mark(summarise(df, a = .Internal(mean(x)))) %>%
      mutate(branch = "0.8.3", fun = ".Internal(mean(.))", n = n, ngroups = ngroups) %>%
      select(branch, fun, n, ngroups, min:last_col())
  }, args = args, libpath = "../bench-libs/0.8.3")

  released_hybrid_mean <- callr::r(function(n, ngroups) {
    library(dplyr, warn.conflicts = FALSE)
    library(purrr)
    library(vctrs)

    df <- tibble(x = rnorm(n), g = sample(rep(1:ngroups, n / ngroups))) %>% group_by(g)
    bench::mark(summarise(df, a = mean(x))) %>%
      mutate(branch = "0.8.3", fun = "hybrid mean(.)", n = n, ngroups = ngroups) %>%
      select(branch, fun, n, ngroups, min:last_col())
  }, args = args, libpath = "../bench-libs/0.8.3")

  released_nonhybrid_mean <- callr::r(function(n, ngroups) {
    library(dplyr, warn.conflicts = FALSE)
    library(purrr)
    library(vctrs)

    mean2 <- function(x, ...) UseMethod("mean")

    df <- tibble(x = rnorm(n), g = sample(rep(1:ngroups, n / ngroups))) %>% group_by(g)
    bench::mark(summarise(df, a = mean2(x))) %>%
      mutate(branch = "0.8.3", fun = "mean2(.)", n = n, ngroups = ngroups) %>%
      select(branch, fun, n, ngroups, min:last_col())
  }, args = args, libpath = "../bench-libs/0.8.3")

  as_tibble(vec_rbind(master_length, master_internal, master_mean, released_internal, released_hybrid_mean, released_nonhybrid_mean))
}

bench_mean(1e6, 10)
bench_mean(1e6, 100000)
