context("Equivalence (manip)") 

df <- data.frame(x = 5:1, y = 1:5)

srcs <- temp_srcs(c("df", "dt", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)

test_that("mutate happens before summarise", {
  # FIXME: only needed because postgresql returns integer for sum
  compare_tbls(tbls, function(x) {
    mutate(x, z = x + y) %.% summarise(sum_z = sum(z))
  }, compare = equal_data_frame, convert = TRUE)
})

test_that("select operates on mutated vars", {
  compare_tbls(tbls, function(x) mutate(x, z = x + y) %.% select(z))
})
