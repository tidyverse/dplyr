context("Mutate")

test_that("repeated outputs applied progressively (data frame)", {
  df <- data.frame(x = 1)
  out <- mutate(df, z = x + 1, z = z + 1)  
  
  expect_equal(nrow(out), 1)
  expect_equal(ncol(out), 2)
  
  expect_equal(out$z, 3)
})

test_that("repeated outputs applied progressively (grouped_df)", {
  df <- data.frame(x = c(1, 1), y = 1:2)
  ds <- group_by(df, y)
  out <- mutate(ds, z = x + 1, z = z + 1)  
  
  expect_equal(nrow(out), 2)
  expect_equal(ncol(out), 3)
  
  expect_equal(out$z, c(3L, 3L))
})

df <- data.frame(x = 1:10, y = 6:15)
srcs <- temp_srcs(c("df", "dt", "cpp", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)

test_that("two mutates equivalent to one", {
  compare_tbls(tbls, function(tbl) tbl %.% mutate(x2 = x * 2, y4 = y * 4))
})

test_that("mutate can refer to variables that were just created (#140)", {
  res <- mutate(tbl_cpp(mtcars), cyl1 = cyl + 1, cyl2 = cyl1 + 1)
  expect_equal( res$cyl2, mycars$cyl+2)
  
  gmtcars <- group_by( tbl_cpp(mtcars), am)
  res <- mutate(gmtcars, cyl1 = cyl + 1, cyl2 = cyl1 + 1)
  res_direct <- mutate(gmtcars, cyl2 = cyl + 2 )
  expect_equal( res$cyl2, res_direct$cyl2 )
})
