context("Equivalence (joins)") 

dates <- Sys.Date()+1:4
times <- Sys.time()+1:4

c <- data.frame(
  x = c(1, 1, 2, 3), 
  y = c(1, 1, 2, 3), 
  a = 1:4, 
  d = dates, 
  t = times 
  )
d <- data.frame(
  x = c(1, 2, 2, 4), 
  y = c(1, 2, 2, 4),
  b = 1:4, 
  dd = dates, 
  tt = times 
  )

srcs <- temp_srcs(c("df", "dt", "cpp", "sqlite", "postgres"))
tbls <- temp_load(srcs, list(c = c, d = d))

test_that("inner join equivalent across all tbls", {
  compare_tbls(tbls, function(x) inner_join(x$c, x$d, by = c("x", "y")))
})

test_that("left join equivalent across all tbls", {
  compare_tbls(tbls, function(x) left_join(x$c, x$d, by = c("x", "y")))
})

test_that("semi join equivalent across all tbls", {
  compare_tbls(tbls, function(x) semi_join(x$c, x$d, by = c("x", "y")))
})

test_that("anti join equivalent across all tbls", {
  compare_tbls(tbls, function(x) anti_join(x$c, x$d, by = c("x", "y")))
})
