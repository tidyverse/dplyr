library(microbenchmark)

n <- 1e5
df <- data.frame(matrix(runif(n * 100), ncol = 100))
x <- df[[1]]

x_ran <- sample(n, 1e3)
x_ord <- sort(x_ran)
x_beg <- 1:1e3
x_end <- (n - 1e3 + 1):n

microbenchmark(
  df[x_ran, ],
  df[x_ord, ],
  df[x_beg, ],
  df[x_end, ]
)

microbenchmark(
  x[x_ran],
  x[x_ord],
  x[x_beg],
  x[x_end]
)

# Sequential access is fastest (beg =~ end)
# Ordered randomly slightly faster than not ordered

# What's the fastest way to extract a subset of a single col?
microbenchmark(
  df[x_ran, 1],
  df[[1]][x_ran],
  .subset2(df, 1)[x_ran],
  .subset2(df, "X1")[x_ran],
  df$X1[x_ran],
  df[["X1"]][x_ran],
  x[x_ran]
)

# Using .subset2 is only fractionally slower than subsetting vector.
# About 20% faster as next best, df$X1[x_ran].  Twice as fast as df[[1]][x_ran]

# How do things change if we use a list instead of a data frame?
lst <- as.list(df)
microbenchmark(
  lst[[1]][x_ran],
  .subset2(lst, 1)[x_ran],
  lst$X1[x_ran],
  lst[["X1"]][x_ran],
  x[x_ran]
)
# Effectively no difference for lists - all as fast as the fastest
# data.frame methods.  Difference probably due to S3 dispatch for data frames.
