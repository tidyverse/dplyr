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
  expect_equal(ncol(out), 3)
  
  expect_equal(out$z, c(3L, 3L))
})
