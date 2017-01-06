set.seed(123)

ALPHABET <- letters[1:4]
ALPHABET <- letters[1:10]
ALPHABET <- letters

create_ids <- function(N) {
  s <- paste(sample(c(ALPHABET, "|"), N, replace = TRUE), collapse = "")
  ss <- strsplit(s, "|", fixed = TRUE)[[1]]
  ss <- unique(ss)
  ss <- ss[nchar(ss) > 3]
  ss
}

N <- 1e4
ids <- create_ids(N)

benchmark <- function(ids, summarize) {
  force(ids)
  df <- data_frame(ids, n = 0)
  gc()
  if (summarize) {
    system.time(group_by(df, ids) %>% summarize(n = mean(n)))
  } else {
    system.time(group_by(df, ids))
  }
}

devtools::load_all()

# master:
#
# > # benchmark(ids, TRUE)
# > # benchmark(sample(ids, NN, replace = FALSE), TRUE)
# > # benchmark(sample(ids, NN, replace = TRUE), TRUE)
# > benchmark(ids, F .... [TRUNCATED]
# user  system elapsed
# 4.440   0.032   4.469
#
# > benchmark(sample(ids, NN, replace = FALSE), FALSE)
# user  system elapsed
# 2.164   0.000   2.166
#
# > benchmark(sample(ids, NN, replace = TRUE), FALSE)
# user  system elapsed
# 3.176   0.000   3.175

# f:
# > benchmark(ids, TRUE)
# user  system elapsed
# 2.500   0.024   2.522
#
# > benchmark(sample(ids, NN, replace = FALSE), TRUE)
# user  system elapsed
# 2.320   0.000   2.319
#
# > benchmark(sample(ids, NN, replace = TRUE), TRUE)
# user  system elapsed
# 2.584   0.000   2.584


NN <- 3e2

#gprofiler::start_profiler()

benchmark(ids, TRUE)

#gprofiler::stop_profiler()
#gprofiler::show_profiler_pdf()

#benchmark(sample(ids, NN, replace = FALSE), TRUE)
# benchmark(sample(ids, NN, replace = TRUE), TRUE)
# benchmark(ids, FALSE)
# benchmark(sample(ids, NN, replace = FALSE), FALSE)
# benchmark(sample(ids, NN, replace = TRUE), FALSE)
