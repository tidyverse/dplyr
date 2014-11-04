context("Mutate - windowed")

df <- data.frame(x = 1:10, y = seq(1,10,by=1), g = rep(c(1, 2), each = 5))
srcs <- temp_srcs("df", "dt", "postgres")
tbls <- temp_load(srcs, df)

test_that("mutate calls windowed versions of sql functions", {
  compare_tbls(tbls, function(x) {
    x %>% group_by(g) %>% mutate(r = as.numeric(row_number(x)))
  })
})

test_that("recycled aggregates generate window function", {
  compare_tbls(tbls, function(x) {
    x %>% group_by(g) %>% mutate(r = x > mean(x))
  })
})

test_that("cumulative aggregates generate window function", {
  compare_tbls(tbls, function(x) {
    x %>% group_by(g) %>% mutate(cx = as.integer(cumsum(x)))
  })
})

test_that("desc is correctly handled by window functions", {
  expect_equal(mutate(df, rank=min_rank(desc(x)) )$rank, 10:1 )
  expect_equal(mutate(group_by(df,g), rank=min_rank(desc(x)))$rank, rep(5:1,2) )

  expect_equal(mutate(df, rank=row_number(desc(x)) )$rank, 10:1 )
  expect_equal(mutate(group_by(df,g), rank=row_number(desc(x)))$rank, rep(5:1,2) )

})

test_that("row_number gives correct results",{
  tmp <- data.frame(id=rep(c(1,2),each=5),value=c(1,1,2,5,0,6,4,0,0,2))
  res <- group_by(tmp, id) %>% mutate(var=row_number(value))
  expect_equal(res$var, c(2,3,4,5,1,5,4,1,2,3))
})

test_that("row_number works with 0 arguments", {
  g <- group_by(mtcars, cyl)
  expect_equal( mutate( g, rn = row_number() ), mutate( g, rn = 1:n() ) )
})

test_that("cum(sum,min,max) works", {
  res <- mutate( df,
    csumx = cumsum(x), csumy = cumsum(y),
    cminx = cummin(x), cminy = cummin(y),
    cmaxx = cummax(x), cmaxy = cummax(y)
  )
  expect_equal( res$csumx, cumsum(df$x) )
  expect_equal( res$csumy, cumsum(df$y) )
  expect_equal( res$cminx, cummin(df$x) )
  expect_equal( res$cminy, cummin(df$y) )
  expect_equal( res$cmaxx, cummax(df$x) )
  expect_equal( res$cmaxy, cummax(df$y) )

  res <- mutate( group_by(df,g) ,
    csumx = cumsum(x), csumy = cumsum(y),
    cminx = cummin(x), cminy = cummin(y),
    cmaxx = cummax(x), cmaxy = cummax(y)
    )
  expect_equal( res$csumx, c( cumsum(df$x[1:5]), cumsum(df$x[6:10]) ) )
  expect_equal( res$csumy, c( cumsum(df$y[1:5]), cumsum(df$y[6:10]) ) )
  expect_equal( res$cminx, c( cummin(df$x[1:5]), cummin(df$x[6:10]) ) )
  expect_equal( res$cminy, c( cummin(df$y[1:5]), cummin(df$y[6:10]) ) )
  expect_equal( res$cmaxx, c( cummax(df$x[1:5]), cummax(df$x[6:10]) ) )
  expect_equal( res$cmaxy, c( cummax(df$y[1:5]), cummax(df$y[6:10]) ) )

  df$x[3] <- NA
  df$y[4] <- NA
  res <- mutate( df,
    csumx = cumsum(x), csumy = cumsum(y),
    cminx = cummin(x), cminy = cummin(y),
    cmaxx = cummax(x), cmaxy = cummax(y)
  )
  expect_true( all(is.na(res$csumx[3:10])) )
  expect_true( all(is.na(res$csumy[4:10])) )

  expect_true( all(is.na(res$cminx[3:10])) )
  expect_true( all(is.na(res$cminy[4:10])) )

  expect_true( all(is.na(res$cmaxx[3:10])) )
  expect_true( all(is.na(res$cmaxy[4:10])) )

})

test_that( "lead and lag simple hybrid version gives correct results (#133)", {
  res <- group_by(mtcars, cyl) %>%
    mutate( disp_lag_2 = lag(disp, 2), disp_lead_2 = lead(disp, 2) ) %>% 
    summarise( 
        lag1  = all(is.na(head(disp_lag_2, 2))), 
        lag2  = all(!is.na(tail(disp_lag_2, -2))),
        
        lead1 = all(is.na(tail(disp_lead_2, 2))), 
        lead2 = all(!is.na(head(disp_lead_2, -2)))
        
    )
  expect_true( all(res$lag1) )
  expect_true( all(res$lag2) )
  
  expect_true( all(res$lead1) )
  expect_true( all(res$lead2) )
})

test_that("min_rank handles columns full of NaN (#726)", {
  test <- data.frame( 
    Name = c("a", "b","c", "d", "e"),
    ID = c(1, 1, 1, 1, 1),
    expression = c(NaN, NaN, NaN, NaN, NaN)
  )
  data <- group_by(test, ID) %>% mutate(rank = min_rank(expression) )
  expect_true( all(data$rank == 1L ) )
})

# FIXME: this should only fail if strict checking is on.
# test_that("window functions fail if db doesn't support windowing", {
#   df_sqlite <- temp_load(temp_srcs("sqlite"), df)$sql %>% group_by(g)
#   ok <- collect(df_sqlite %>% mutate(x > 4))
#   expect_equal(nrow(ok), 10)
#
#   expect_error(df_sqlite %>% mutate(x > mean(x)), "does not support")
#   expect_error(df_sqlite %>% mutate(r = row_number()), "does not support")
# })
