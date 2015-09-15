context("cbind")

test_that( "cbind uses shallow copies", {
  df1 <- data.frame(
    int = 1:10,
    num = rnorm(10),
    cha = letters[1:10],
    stringsAsFactors = FALSE )
  df2 <- data.frame(
    log = sample(c(T,F), 10, replace = TRUE),
    dat = seq.Date( Sys.Date(), length.out = 10, by = "day" ),
    tim = seq( Sys.time(), length.out = 10, by = "1 hour" )
    )
  df <- bind_cols(df1, df2)

  expect_equal( dfloc(df1), dfloc(df)[names(df1)] )
  expect_equal( dfloc(df2), dfloc(df)[names(df2)] )
})

test_that( "bind_cols produces a tbl_df (#779)", {
  df1 <- data.frame(a = letters)
  df2 <- data.frame(A = LETTERS)

  expect_is( bind_cols(df1, df2), "tbl_df")
  expect_is( bind_cols(df1, tbl_df(df2)), "tbl_df")
  expect_is( bind_cols(tbl_df(df1), df2), "tbl_df")
  expect_is( bind_cols(tbl_df(df1), tbl_df(df2)), "tbl_df")

})

test_that("bind_cols can handle lists (#1104)", {
  my_list <- list(list(x = 1, y = 'a'), list(z = 2))
  res <- bind_cols(my_list)
  expect_equal(nrow(res), 1L)
  expect_equal(ncol(res), 3L)
  expect_is(res$x, "numeric")
  expect_is(res$y, "character")
  expect_is(res$z, "numeric")

  res <- bind_cols(list(x = 1, y = 'a'), list(z = 2))
  expect_equal(nrow(res), 1L)
  expect_equal(ncol(res), 3L)
  expect_is(res$x, "numeric")
  expect_is(res$y, "character")
  expect_is(res$z, "numeric")
})

test_that( "bind_cols skips NULL (#1148)", {
  d1 <- data_frame( x = 1:10 )
  d2 <- data_frame( y = letters[1:10] )

  res <- bind_cols(NULL, d1, d2)
  expect_equal( ncol(res), 2L )
  expect_equal( res$x, d1$x)
  expect_equal( res$y, d2$y)

  res <- bind_cols(d1, NULL, d2)
  expect_equal( ncol(res), 2L )
  expect_equal( res$x, d1$x)
  expect_equal( res$y, d2$y)

  res <- bind_cols(d1, d2, NULL)
  expect_equal( ncol(res), 2L )
  expect_equal( res$x, d1$x)
  expect_equal( res$y, d2$y)
})
