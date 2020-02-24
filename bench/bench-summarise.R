library(dplyr)
library(bench)

# setup code
df <- tibble(x = rnorm(1e6), y = rnorm(1e6), g = sample(rep(1:1e4, 100))) %>% group_by(g)

bench::mark(
  # constant
  summarise(df, n = 42),

  # hybrid
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

  # non hybrid
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

  check = FALSE
)
