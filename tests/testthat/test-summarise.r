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
srcs <- temp_srcs(c("df", "dt", "cpp", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)

test_that("summarise peels off a single layer of grouping", {
  for (i in seq_along(tbls)) {
    grouped <- group_by(tbls[[i]], x, y)
    summed <- summarise(grouped, n())
    
    expect_equal(unname(groups(summed)), list(quote(x)), info = names(tbls)[i])
  }
})

test_that("summarise can refer to variables that were just created (#138)", {
  res <- summarise(tbl_cpp(mtcars), cyl1 = mean(cyl), cyl2 = cyl1 + 1  )
  expect_equal(res$cyl2, mean(mtcars$cyl)+1)
  
  gmtcars <- group_by(tbl_cpp(mtcars), am)
  res <- summarise(gmtcars, cyl1 = mean(cyl), cyl2 = cyl1 + 1) 
  res_direct <- summarise(gmtcars, cyl2 = mean(cyl) + 1) 
  expect_equal(res$cyl2, res_direct$cyl2)
})

test_that("summarise refuses to modify grouping variable (#143)", {
  df <- data.frame( a = c(1,2,1,2), b = c(1,1,2,2), x = 1:4 )
  ds <- group_by(tbl_cpp(df), a, b)
  expect_error( 
    summarise(ds, a = mean(x), a = b + 1),
    "cannot modify grouping variable"
  )
})

