context("tbl_sql")

srcs <- temp_srcs("sqlite", "postgres", "oracle")

df <- data.frame(x = 1:3, y = 3:1)
tbls <- temp_load(srcs, list(df = df))
name <- tbls[[1]]$df$from

test_that("can generate sql tbls with raw sql", {
  
  raw <- lapply(srcs, function(x) tbl(x, build_sql("SELECT * FROM ", name)))
  expect_true(compare_tbls(raw, identity, ref = df))
})