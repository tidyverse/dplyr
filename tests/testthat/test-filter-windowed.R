context("Filter - windowed")

df <- data.frame(x = 1:10, g = rep(c(1, 2), each = 5))
srcs <- temp_srcs("df", "dt", "postgres", "oracle")
tbls <- temp_load(srcs, df)

test_that("filter calls windowed versions of sql functions", {
  compare_tbls(tbls, function(x) {
    x %>% group_by(g) %>% filter(row_number(x) < 3)
  })
})

test_that("recycled aggregates generate window function", {
  compare_tbls(tbls, function(x) {
    x %>% group_by(g) %>% filter(x > mean(x))
  })
})

test_that("cumulative aggregates generate window function", {
  compare_tbls(tbls, function(x) {
    x %>% group_by(g) %>% filter(cumsum(x) > 10)
  })
})
