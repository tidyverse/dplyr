context("Equivalence (manip)") 

df <- data.frame(x = 5:1, y = 1:5)

srcs <- temp_srcs(c("df", "dt", "cpp", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)

# FIXME: only needed because postgresql returns integer for sum
int_to_num <- function(x, y) {
  is_integer_x <- vapply(x, is.integer, logical(1))
  is_integer_y <- vapply(y, is.integer, logical(1))
  
  x[is_integer_x] <- lapply(x[is_integer_x], as.numeric)
  y[is_integer_y] <- lapply(y[is_integer_y], as.numeric)
  
  equal_data_frame(x, y)
}

test_that("mutate happens before summarise", {
  compare_tbls(tbls, function(x) {
    mutate(x, z = x + y) %.% summarise(sum_z = sum(z))
  }, compare = int_to_num)
})

test_that("select operates on mutated vars", {
  compare_tbls(tbls, function(x) mutate(x, z = x + y) %.% select(z))
})
