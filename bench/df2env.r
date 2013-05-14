# What's the fastest way to get a subset of the colums in
# a data frame into an environment?

# list2env is extremely fast so - 20 µs for 200 cols
# and doesn't seem to grow with underlying data
# so don't worry about it - just use list2env

# Just copy them all
l1 <- function(df, cols) {
  list2env(df, parent = emptyenv())
}

# Subset, then copy
l2 <- function(df, cols) {
  list2env(df[cols], parent = emptyenv())
}

# Copy by hand
l3 <- function(df, cols) {
  env <- new.env(parent = emptyenv())
  for (col in cols) {
    env[[col]] <- .subset2(df, col)
  }
  env
}


library(microbenchmark)
options(digits = 3)
data(baseball, package = "plyr")

all <- names(baseball)
print(microbenchmark(
  l1(baseball, all),
  l2(baseball, all),
  l3(baseball, all)
))

long <- do.call("rbind", replicate(10, baseball, simplify = FALSE))
print(microbenchmark(
  l1(long, all),
  l3(long, all),
  l1(long, c("id", "sf")),
  l3(long, c("id", "sf"))
))

wide <- do.call("cbind", replicate(10, baseball, simplify = FALSE))
print(microbenchmark(
  l1(wide, all),
  l3(wide, all),
  l1(wide, c("id", "sf")),
  l3(wide, c("id", "sf"))
))
