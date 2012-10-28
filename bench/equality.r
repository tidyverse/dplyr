library(microbenchmark)

x <- 1

microbenchmark(
  identical(x, 1),
  identical(x, 2),
  x == 1,
  x == 2,
  x == 1L,
  x == 2L
)

# Identical is ~6x slower

microbenchmark(
  if (identical(x, 1)) TRUE else FALSE,
  if (identical(x, 2)) TRUE else FALSE,
  if (x == 1) TRUE else FALSE,
  if (x == 2) TRUE else FALSE
)

# Add if adds equal overhead to both
