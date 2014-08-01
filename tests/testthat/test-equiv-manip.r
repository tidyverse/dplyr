context("Equivalence (manip)")

df <- data.frame(x = 5:1, y = 1:5)

srcs <- temp_srcs(c("df", "dt", "sqlite", "postgres", "JDBC_postgres"))
tbls <- temp_load(srcs, df)

test_that("mutate happens before summarise", {
  # FIXME: convert only needed because postgresql returns integer for sum
  compare_tbls(tbls, function(x) {
    mutate(x, z = x + y) %>% summarise(sum_z = sum(z))
  }, compare = equal_data_frame, convert = TRUE)
})

test_that("select operates on mutated vars", {
  # FIXME: convert only needed because JDBC returns numeric for select
  compare_tbls(tbls, function(x) mutate(x, z = x + y) %>% select(z), convert = TRUE)
})
