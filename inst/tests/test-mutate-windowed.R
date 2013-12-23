context("Mutate - windowed")

df <- data.frame(x = 1:10, g = rep(c(1, 2), each = 5))
srcs <- temp_srcs("df", "dt", "postgres")
tbls <- temp_load(srcs, df)

test_that("mutate calls windowed versions of sql functions", {
  compare_tbls(tbls, function(x) {
    x %.% group_by(g) %.% mutate(r = as.numeric(row_number(x)))
  })
})

test_that("recycled aggregates generate window function", {
  compare_tbls(tbls, function(x) {
    x %.% group_by(g) %.% mutate(r = x > mean(x))
  })
})

test_that("cumulative aggregates generate window function", {
  compare_tbls(tbls, function(x) {
    x %.% group_by(g) %.% mutate(cx = as.integer(cumsum(x)))
  })
})

test_that("desc is correctly handled by window functions", {
  expect_equal(mutate(df, rank=min_rank(desc(x)) )$rank, 10:1 )
  expect_equal(mutate(group_by(df,g), rank=min_rank(desc(x)))$rank, rep(5:1,2) )
  
  expect_equal(mutate(df, rank=row_number(desc(x)) )$rank, 10:1 )
  expect_equal(mutate(group_by(df,g), rank=row_number(desc(x)))$rank, rep(5:1,2) )
  
})

# FIXME: this should only fail if strict checking is on.
# test_that("window functions fail if db doesn't support windowing", {
#   df_sqlite <- temp_load(temp_srcs("sqlite"), df)$sql %.% group_by(g)
#   ok <- collect(df_sqlite %.% mutate(x > 4))
#   expect_equal(nrow(ok), 10)
#   
#   expect_error(df_sqlite %.% mutate(x > mean(x)), "does not support")
#   expect_error(df_sqlite %.% mutate(r = row_number()), "does not support")
# })