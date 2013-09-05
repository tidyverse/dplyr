context("Summarise")

test_that("repeated outputs applied progressively", {
  df <- data.frame(x = 5)
  
  out <- summarise(df, x = mean(x), x = x + 1)
  expect_equal(nrow(out), 1)
  expect_equal(ncol(out), 1)
  
  expect_equal(out$x, 6)
})

test_that("repeated outputs applied progressively (grouped_df)", {
  df <- data.frame(x = c(1, 1), y = 1:2)
  ds <- group_by(df, y)
  out <- summarise(ds, z = mean(x), z = z + 1)  
  
  expect_equal(nrow(out), 2)
  expect_equal(ncol(out), 2)
  
  expect_equal(out$z, c(2L, 2L))
})


df <- data.frame(x = rep(1:4, each = 4), y = rep(1:2, each = 8), z = runif(16))
tbls <- clone_tbls(df)

test_that("summarise peels off a single layer of grouping", {
  for (tbl in tbls) {
    grouped <- group_by(tbl, x, y)
    summed <- summarise(grouped, n())
    
    expect_equal(unname(groups(summed)), list(quote(x)))
  }
})