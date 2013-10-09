context("Equivalence (manip)") 

df <- data.frame(x = 5:1, y = 1:5)
tbls <- clone_tbls(df)

test_that("mutate happens before summarise", {
  mutate_summarise <- function(x) {
    mutate(x, z = x + y) %.% summarise(sum_z = sum(z)) %.% strip()
  }
  res <- lapply(tbls, mutate_summarise)
  
  expect_equal(res$dt, res$df)
  expect_equal(res$sqlite, res$df)
  expect_equal(res$postgres, res$df)
})


test_that("select operates on mutated vars", {
  mutate_select <- function(x) {
    mutate(x, z = x + y) %.% select(z) %.% strip()
  }
  res <- lapply(tbls, mutate_select)
  
  expect_equal(res$dt, res$df)
  expect_equal(res$sqlite, res$df)
  expect_equal(res$postgres, res$df)
})
