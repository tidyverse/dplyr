library(dplyr)
library(bench)

df <- tibble(x = rnorm(1e6), g = sample(rep(1:1e4, 100)), f = factor(sample(rep(1:1e4, 100))))

bench::mark(
  group_by(df, x),
  group_by(df, g),
  group_by(df, f),

  group_by(df, x, f),
  group_by(df, f, x),

  check = FALSE
)
