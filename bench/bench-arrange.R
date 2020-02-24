library(dplyr)
library(bench)

# setup code
df <- tibble(x = rnorm(1e6), y = rnorm(1e6), g = sample(rep(1:1e4, 100))) %>% group_by(g)

bench::mark(
  # simple caclulation not involving the columns
  mutate(df, z = 42.0),

  # simple calculation involving 1 column
  mutate(df, z = x + 0),

  # simple calculation involving 2 columns
  mutate(df, z = x + y),

  # hybrid + window
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

  # hybrid + resize to match
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
  mutate(df, x = sum(x, na.rm = TRUE)),

  check = FALSE
)
