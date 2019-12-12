library(dplyr)
library(rlang)
library(purrr)
library(tibble)
library(tidyr)

if (!dir.exists("../bench-libs")) dir.create("../bench-libs")

if (!dir.exists("../bench-libs/0.8.3")) {
  dir.create("../bench-libs/0.8.3")
  pak::pkg_install("dplyr", lib = "../bench-libs/0.8.3", ask = FALSE)
}

libs <- list.files("../bench-libs", full.names = TRUE)
names(libs) <- basename(libs)

libs <- c(libs, "master" = .libPaths())

benchs <- function(libs, setup, ..., iterations = NULL){
  dots <- rlang::exprs(...)
  setup <- substitute(setup)

  f <- function(){}
  body(f) <- rlang::expr({
    library(dplyr, warn.conflicts = FALSE)
    !!setup
    bench::mark(!!!dots, check = FALSE, iterations = !!iterations) %>%
      mutate(expression = purrr::map_chr(expression, deparse))
  })
  results <- purrr::imap(libs, ~callr::r(f, libpath = .x) %>% mutate(version = .y))
  as_tibble(vctrs::vec_rbind(!!!results))
  # %>%
  #   select(expression, version, median) %>%
  #   pivot_wider(names_from = version, values_from = median)
}

summarise_hybrid <- benchs(
  iterations = 10,
  libs = libs,

  setup = {
    df <- tibble(x = rnorm(1e5), g = sample(rep(1:1e3, 100))) %>% group_by(g)
  },

  summarise(df, n = n()),
  summarise(df, x = n_distinct(x)),

  summarise(df, x = first(x)),
  summarise(df, x = last(x)),
  summarise(df, x = nth(x, n = 1L)),

  summarise(df, x = first(x, default = 42)),
  summarise(df, x = last(x, default = 42)),
  summarise(df, x = nth(x, n = 1L, default = 42)),


  summarise(df, x = mean(x)),
  summarise(df, x = mean(x, na.rm = TRUE)),

  summarise(df, x = sd(x)),
  summarise(df, x = sd(x, na.rm = TRUE)),

  summarise(df, x = var(x)),
  summarise(df, x = var(x, na.rm = TRUE)),

  summarise(df, x = min(x)),
  summarise(df, x = min(x, na.rm = TRUE)),

  summarise(df, x = max(x)),
  summarise(df, x = max(x, na.rm = TRUE)),

  summarise(df, x = sum(x)),
  summarise(df, x = sum(x, na.rm = TRUE)),

) %>%
  mutate(hybrid = TRUE)

summarise_non_hybrid <- benchs(
  iterations = 10,
  libs = libs,

  setup = {
    df <- tibble(x = rnorm(1e4), g = sample(rep(1:1e2, 100))) %>% group_by(g)
  },

  summarise(df, n = 0 + 0),
  summarise(df, n = 0+n()),
  summarise(df, x = 0+n_distinct(x)),

  summarise(df, x = 0+first(x)),
  summarise(df, x = 0+last(x)),
  summarise(df, x = 0+nth(x, n = 1L)),

  summarise(df, x = 0+first(x, default = 42)),
  summarise(df, x = 0+last(x, default = 42)),
  summarise(df, x = 0+nth(x, n = 1L, default = 42)),


  summarise(df, x = 0+mean(x)),
  summarise(df, x = 0+mean(x, na.rm = TRUE)),

  summarise(df, x = 0+sd(x)),
  summarise(df, x = 0+sd(x, na.rm = TRUE)),

  summarise(df, x = 0+var(x)),
  summarise(df, x = 0+var(x, na.rm = TRUE)),

  summarise(df, x = 0+min(x)),
  summarise(df, x = 0+min(x, na.rm = TRUE)),

  summarise(df, x = 0+max(x)),
  summarise(df, x = 0+max(x, na.rm = TRUE)),

  summarise(df, x = 0+sum(x)),
  summarise(df, x = 0+sum(x, na.rm = TRUE)),

) %>%
  mutate(hybrid = FALSE)

mutate_hybrid <- benchs(
  iterations = 10,
  libs = libs,

  setup = {
    df <- tibble(x = rnorm(1e4), g = sample(rep(1:1e2, 100))) %>% group_by(g)
  },

  # window
  mutate(df, x = lead(x)),
  mutate(df, x = lead(x, n = 1L)),

  mutate(df, x = lag(x)),
  mutate(df, x = lag(x, n = 1L)),

  mutate(df, x = ntile(n = 2)),
  mutate(df, x = ntile(x, n = 2)),

  mutate(df, x = min_rank(x)),
  mutate(df, x = dense_rank(x)),
  mutate(df, x = percent_rank(x)),
  mutate(df, x = cume_dist(x)),

  mutate(df, x = row_number(x)),

  # same as summarise() + recycling to match size
  mutate(df, n = n()),
  mutate(df, x = n_distinct(x)),

  mutate(df, x = first(x)),
  mutate(df, x = last(x)),
  mutate(df, x = nth(x, n = 1L)),

  mutate(df, x = first(x, default = 42)),
  mutate(df, x = last(x, default = 42)),
  mutate(df, x = nth(x, n = 1L, default = 42)),


  mutate(df, x = mean(x)),
  mutate(df, x = mean(x, na.rm = TRUE)),

  mutate(df, x = sd(x)),
  mutate(df, x = sd(x, na.rm = TRUE)),

  mutate(df, x = var(x)),
  mutate(df, x = var(x, na.rm = TRUE)),

  mutate(df, x = min(x)),
  mutate(df, x = min(x, na.rm = TRUE)),

  mutate(df, x = max(x)),
  mutate(df, x = max(x, na.rm = TRUE)),

  mutate(df, x = sum(x)),
  mutate(df, x = sum(x, na.rm = TRUE))
) %>%
  mutate(hybrid = TRUE)


mutate_non_hybrid <- benchs(
  iterations = 10,
  libs = libs,

  setup = {
    df <- tibble(x = rnorm(1e4), g = sample(rep(1:1e2, 100))) %>% group_by(g)
  },

  # window
  mutate(df, x = 0),
  mutate(df, x = 0 + lead(x)),
  mutate(df, x = 0 + lead(x, n = 1L)),

  mutate(df, x = 0 + lag(x)),
  mutate(df, x = 0 + lag(x, n = 1L)),

  mutate(df, x = 0 + ntile(n = 2)),
  mutate(df, x = 0 + ntile(x, n = 2)),

  mutate(df, x = 0 + min_rank(x)),
  mutate(df, x = 0 + dense_rank(x)),
  mutate(df, x = 0 + percent_rank(x)),
  mutate(df, x = 0 + cume_dist(x)),

  mutate(df, x = 0 + row_number(x)),

  # same as summarise() + recycling to match size
  mutate(df, n = 0 + n()),
  mutate(df, x = 0 + n_distinct(x)),

  mutate(df, x = 0 + first(x)),
  mutate(df, x = 0 + last(x)),
  mutate(df, x = 0 + nth(x, n = 1L)),

  mutate(df, x = 0 + first(x, default = 42)),
  mutate(df, x = 0 + last(x, default = 42)),
  mutate(df, x = 0 + nth(x, n = 1L, default = 42)),


  mutate(df, x = 0 + mean(x)),
  mutate(df, x = 0 + mean(x, na.rm = TRUE)),

  mutate(df, x = 0 + sd(x)),
  mutate(df, x = 0 + sd(x, na.rm = TRUE)),

  mutate(df, x = 0 + var(x)),
  mutate(df, x = 0 + var(x, na.rm = TRUE)),

  mutate(df, x = 0 + min(x)),
  mutate(df, x = 0 + min(x, na.rm = TRUE)),

  mutate(df, x = 0 + max(x)),
  mutate(df, x = 0 + max(x, na.rm = TRUE)),

  mutate(df, x = 0 + sum(x)),
  mutate(df, x = 0 + sum(x, na.rm = TRUE))
) %>%
  mutate(hybrid = FALSE)

results <- as_tibble(vctrs::vec_rbind(summarise_hybrid, summarise_non_hybrid, mutate_hybrid, mutate_non_hybrid))


