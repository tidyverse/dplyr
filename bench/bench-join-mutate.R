library(dplyr, warn.conflicts = FALSE)
library(bench)
set.seed(342)

df1 <- tibble(x = rnorm(1e5), a = sample(1:10, 1e5, replace = TRUE))
df2 <- tibble(y = rnorm(1e3), a = sample(1:10, 1e3, replace = TRUE))
df3 <- tibble(x = rnorm(1e5), a = sample(1:10, 1e5, replace = TRUE), b = sample(1:5, 1e5, replace = TRUE))
df4 <- tibble(y = rnorm(1e3), a = sample(1:10, 1e3, replace = TRUE), b = sample(1:5, 1e3, replace = TRUE))

bench::mark(
  inner_join(df1, df2, by = "a"),
  left_join(df1, df2, by = "a"),
  right_join(df1, df2, by = "a"),
  full_join(df1, df2, by = "a"),

  check = FALSE
)

bench::mark(
  inner_join(df3, df4, by = c("a", "b")),
  left_join(df3, df4, by = c("a", "b")),
  right_join(df3, df4, by = c("a", "b")),
  full_join(df3, df4, by = c("a", "b")),

  check = FALSE
)
