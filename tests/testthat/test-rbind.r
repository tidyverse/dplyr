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
  expect_equal( 
    rbind_list( df_var, df_var, df_var) , 
    rbind( df_var, df_var, df_var )
  )
})

test_that("rbind_list reorders columns", {
  columns <- seq_len(ncol(df_var))
  expect_equal( 
    rbind_list( 
      df_var, 
      df_var[, sample(columns)], 
      df_var[, sample(columns)] 
    ),   
    rbind( df_var, df_var, df_var )
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
