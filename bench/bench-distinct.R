library(dplyr, warn.conflicts = FALSE)
library(bench)

df <- tibble(x = rnorm(1e6), g = sample(rep(1:1e4, 100)), f = factor(sample(rep(1:1e4, 100))))

bench::mark(
  distinct(df, x),
  distinct(df, g),
  distinct(df, f),

  distinct(df, x, f),
  distinct(df, f, x),

  check = FALSE
)
