context("Equivalence (joins)") 

c <- data.frame(
  x = c(1, 1, 2, 3), 
  y = c(1, 1, 2, 3), 
  a = 1:4)
d <- data.frame(
  x = c(1, 2, 2, 4), 
  y = c(1, 2, 2, 4),
  b = 1:4)

c_tbls <- clone_tbls(c)
d_tbls <- clone_tbls(d)

test_that("inner join equivalent across all tbls", {
  j_df <- strip(inner_join(c_tbls$df, d_tbls$df, by = c("x", "y")))
  j_dt <- strip(inner_join(c_tbls$dt, d_tbls$dt, by = c("x", "y")))
  j_sqlite <- strip(inner_join(c_tbls$sqlite, d_tbls$sqlite, by = c("x", "y")))
  
  expect_equal(j_dt, j_df)
  expect_equal(j_sqlite, j_df)  
})

test_that("left join equivalent across all tbls", {
  j_df <- strip(left_join(c_tbls$df, d_tbls$df, by = c("x", "y")))
  j_dt <- strip(left_join(c_tbls$dt, d_tbls$dt, by = c("x", "y")))
  j_sqlite <- strip(left_join(c_tbls$sqlite, d_tbls$sqlite, by = c("x", "y")))
  
  expect_equal(j_dt, j_df)
  expect_equal(j_sqlite, j_df)  
})

test_that("semi join equivalent across all tbls", {
  j_df <- strip(semi_join(c_tbls$df, d_tbls$df, by = c("x", "y")))
  j_dt <- strip(semi_join(c_tbls$dt, d_tbls$dt, by = c("x", "y")))
  j_sqlite <- strip(semi_join(c_tbls$sqlite, d_tbls$sqlite, by = c("x", "y")))
  
  expect_equal(j_dt, j_df)
  expect_equal(j_sqlite, j_df)  
})

test_that("anti join equivalent across all tbls", {
  j_df <- strip(anti_join(c_tbls$df, d_tbls$df, by = c("x", "y")))
  j_dt <- strip(anti_join(c_tbls$dt, d_tbls$dt, by = c("x", "y")))
  j_sqlite <- strip(anti_join(c_tbls$sqlite, d_tbls$sqlite, by = c("x", "y")))
  
  expect_equal(j_dt, j_df)
  expect_equal(j_sqlite, j_df)  
})
