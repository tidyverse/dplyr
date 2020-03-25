library(dplyr, warn.conflicts = FALSE)
library(bench)

df <- tibble(x = rnorm(1e6), g = sample(rep(1:1e4, 100)))
gf <- group_by(df, g)

bench::mark(
  filter(df, x == 1),
  filter(gf, x == 1),

  check = FALSE
)
