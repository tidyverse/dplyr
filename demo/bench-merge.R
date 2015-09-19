# Compare base, data table, dplyr and pandas
#
# To install pandas on OS X:
# * brew update && brew install python
# * pip install --upgrade setuptools
# * pip install --upgrade pip
# * pip install pandas

library(dplyr)
library(data.table)
library(microbenchmark)
library(reshape2)
set.seed(1014)

# Generate sample data ---------------------------------------------------------

random_strings <- function(n, m) {
  mat <- matrix(sample(letters, m * n, rep = TRUE), ncol = m)
  apply(mat, 1, paste, collapse = "")
}

N <- 10000
indices  <- random_strings(N, 10)
indices2 <- random_strings(N, 10)

left <- data.frame(
  key = rep(indices[1:8000], 10),
  key2 = rep(indices2[1:8000], 10),
  value = rnorm(80000)
)
right <- data.frame(
  key = indices[2001:10000],
  key2 = indices2[2001:10000],
  value2 = rnorm(8000)
)

write.csv(left, "pandas/left.csv", row.names = FALSE)
write.csv(right, "pandas/right.csv", row.names = FALSE)

# Equivalent functions for each technique --------------------------------------

base <- list(
  setup = function(x, y) list(x = x, y = y),
  
  left  = function(x, y) base::merge(x, y, all.x = TRUE),
  right = function(x, y) base::merge(x, y, all.y = TRUE),
  inner = function(x, y) base::merge(x, y)
)

data.table <- list(
  setup = function(x, y) {
    list(
      x = data.table(x, key = c("key", "key2")),
      y = data.table(y, key = c("key", "key2"))
    )
  },
  
  left  = function(x, y) x[y],
  right = function(x, y) y[x],
  inner = function(x, y) merge(x, y, all = FALSE)
)

dplyr <- list(
  setup = function(x, y) list(x = x, y = y),
  
  left  = function(x, y) left_join(x, y, by = c("key", "key2")),
  right = function(x, y) NULL,
  inner = function(x, y) inner_join(x, y, by = c("key", "key2"))
)

techniques <- list(base = base, data.table = data.table, dplyr = dplyr)

# Aggregate results ------------------------------------------------------------

niter <- 10

r <- lapply(names(techniques), function(nm) {
  tech <- techniques[[nm]]
  df <- tech$setup(left, right)
  m <- microbenchmark(
    left = tech$left(df$x, df$y),
    right = tech$right(df$x, df$y),
    inner = tech$inner(df$x, df$y),
    times = niter
  )
  
  means <- tapply(m$time, m$expr, FUN = mean) / 1e9
  data.frame(type = names(means), mean = means, tech = nm, 
    row.names = NULL, stringsAsFactors = FALSE)
})


system("cd pandas && python bench_merge.py")
pandas_raw <- read.csv("pandas/out.csv")
pandas <- data.frame(type = pandas_raw$X, mean = pandas_raw$dont_sort, tech = "pandas")

options(digits = 3)

all <- c(r, list(pandas))
all <- do.call("rbind", all)
all$mean <- all$mean * 1000 # millisec
all$mean[all$mean < .1] <- NA # NA out dplyr right
dcast(all, tech ~ type, value.var = "mean")
