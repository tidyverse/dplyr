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


