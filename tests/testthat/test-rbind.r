context("rbind")

df_var <- data.frame(
  l = c(T, F, F),
  i = c(1, 1, 2),
  d = Sys.Date() + c(1, 1, 2),
  f = factor(letters[c(1, 1, 2)]),
  n = c(1, 1, 2) + 0.5,
  t = Sys.time() + c(1, 1, 2),
  c = letters[c(1, 1, 2)],
  stringsAsFactors = FALSE
)

test_that("rbind_list works on key types", {
  exp <- tbl_df( rbind( df_var, df_var, df_var ) ) 
  expect_equal(
    rbind_list( df_var, df_var, df_var) ,
    exp
  )
})

test_that("rbind_list reorders columns", {
  columns <- seq_len(ncol(df_var))
  exp <- tbl_df( rbind( df_var, df_var, df_var ) )
  expect_equal(
    rbind_list(
      df_var,
      df_var[, sample(columns)],
      df_var[, sample(columns)]
    ),
    exp
  )
})

test_that("rbind_list promotes integer to numeric", {
  df  <- data.frame( a = 1:5, b = 1:5 )
  df2 <- df
  df2$a <- as.numeric(df$a)

  res <- rbind_list( df, df2)
  expect_equal( typeof(res$a), "double" )
  expect_equal( typeof(res$b), "integer" )
})

test_that("rbind_list promotes factor to character", {
  df  <- data.frame( a = letters[1:5], b = 1:5, stringsAsFactors=TRUE )
  df2 <- df
  df2$a <- as.character(df$a)

  res <- rbind_list( df, df2)
  expect_equal( typeof(res$a), "character" )
})

test_that("rbind_list doesn't promote factor to numeric", {
  df1 <- data.frame( a = 1:5, b = 1:5 )
  df2 <- data.frame( a = 1:5, b = factor(letters[1:5]) )

  expect_error(rbind_list( df1, df2 ), "not compatible")
})

test_that("rbind_list doesn't coerce integer to factor", {
  df1 <- data.frame( a = 1:10, b = 1:10 )
  df2 <- data.frame( a = 1:5, b = factor(letters[1:5]) )

  expect_error( rbind_list( df1, df2 ), "not compatible" )
})

test_that( "rbind_list coerces factor to character when levels don't match", {
  df1 <- data.frame( a = 1:3, b = factor(c("a", "b", "c")))
  df2 <- data.frame( a = 1:3, b = factor(c("a", "b", "c"),
      levels = c("b", "c", "a", "d")))

  expect_warning(res <- rbind_list( df1, df2 ),
    "Unequal factor levels: coercing to character")
  expect_equal( res$b, c("a","b","c", "a","b","c" ) )
})

test_that( "rbind handles NULL",{
  x <- cbind(a=1:10,b=1:10)
  y <- data.frame(x)
  res <- rbind_all(list(y,y,NULL,y))
  expect_equal(nrow(res), 30L)
})

test_that( "rbind handles NA in factors #279", {
  xx <- as.data.frame(list(a=as.numeric(NA), b="c", c="d"))
  zz <- as.data.frame(list(a=1, b=as.character(NA), c="b"))
  expect_warning( res <- rbind_list( xx, zz ) )

  expect_equal(res$a, c(NA,1.0))
  expect_equal(res$b, c("c", NA))
  expect_equal(res$c, c("d","b"))

})

test_that( "rbind_all only accepts data frames #288",{
  ll <- list(c(1,2,3,4, 5), c(6, 7, 8, 9, 10))
  expect_error(rbind_all(ll))
})

test_that( "rbind propagates timezone for POSIXct #298", {
  dates1 <- data.frame(ID=c("a", "b", "c"), 
                     dates=structure(c(-247320000, -246196800, -245073600), 
                                     tzone = "GMT",
                                     class = c("POSIXct", "POSIXt")), 
                     stringsAsFactors=FALSE)
  
  dates2 <- data.frame(ID=c("d", "e", "f"), 
                       dates=structure(c(-243864000, -242654400, -241444800), 
                                       tzone = "GMT",
                                       class = c("POSIXct", "POSIXt")), 
                       stringsAsFactors=FALSE)
  
  alldates <- rbind_list(dates1, dates2)
  expect_equal( attr( alldates$dates, "tzone" ), "GMT" )
})

test_that( "Collecter_Impl<REALSXP> can collect INTSXP. #321", {
  res <- rbind_list(data.frame(x=0.5), data.frame(x=1:3))
  expect_equal( res$x, c(0.5, 1:3) )
})

test_that( "Collecter_Impl<INTSXP> can collect LGLSXP. #321", {
  res <-  rbind_list(data.frame(x=1:3), data.frame(x=NA))
  expect_equal( res$x, c(1:3, NA) )
})

test_that("rbind_all handles list columns (#463)", {
  dfl <- data.frame(x = I(list(1:2, 1:3, 1:4)))
  res <- rbind_all(list(dfl, dfl))
  expect_equal(rep(dfl$x,2L), res$x)
})

test_that("rbind_all creates tbl_df object", {
  res <- rbind_list(tbl_df(mtcars))
  expect_is( res, "tbl_df" )  
})

test_that("string vectors are filled with NA not blanks before collection (#595)", {
  one <- mtcars[1:10, -10]
  two <- mtcars[11:32, ]
  two$char_col <- letters[1:22]
  
  res <- rbind_list(one, two)
  expect_true( all(is.na(res$char_col[1:10])) )  
})

