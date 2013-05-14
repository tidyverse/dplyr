

e <- new.env()
e$a <- 10

l <- as.list(e)

library(microbenchmark)
options(digits = 3)

print(microbenchmark(
  e[["a"]],
  e$a,
  l[["a"]],
  .subset2(l, "a"),
  get("a", e),
  eval(quote(a), e),
  eval(as.name("a"), e)
))

# Indexing environment or list is much faster than anything indirect.
# Environment slightly faster than list, but only by ~50 ns
