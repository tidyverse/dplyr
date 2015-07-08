context("Filter")

df <- expand.grid(a = 1:10, b = letters[1:10],
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)

srcs <- temp_srcs(c("df", "dt", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)

test_that("filter results independent of data tbl (simple)", {
  expected <- df[df$a > 6, , drop = FALSE]
  compare_tbls(tbls[c("df","sqlite")], function(x) {
    filter_(x, ~ a > 6)
  }, expected)
})

test_that("filter captures local variables", {
  sel <- c("d", "g", "a")
  expected <- df[df$b %in% sel, , drop = FALSE]

  compare_tbls(tbls, function(x) x %>% filter(b %in% sel), ref = expected)
})

test_that("two filters equivalent to one", {
  expected <- filter(df, a > 4 & b == "a")

  compare_tbls(tbls, function(x) x %>% filter(a > 4) %>% filter(b == "a"),
    ref = expected)
})

test_that("filter fails if inputs incorrect length (#156)", {
  expect_error( filter(tbl_df(mtcars), c(F, T)) )
  expect_error( filter(group_by(mtcars, am), c(F, T)) )
})

test_that("filter gives useful error message when given incorrect input", {
  expect_error( filter(tbl_df(mtcars), x ), "unknown column" )
})

test_that("filter handles passing ...", {
  df <- data.frame( x = 1:4 )

  f <- function(...){
    x1 <- 4
    f1 <- function(y) y
    filter(df, ..., f1(x1) > x)
  }
  g <- function(...){
    x2 <- 2
    f(x > x2, ...)
  }
  res <- g()
  expect_equal( res$x, 3L )

  df <- group_by(df,x)
  res <- g()
  expect_equal( res$x, 3L )

})

test_that( "filter handles simple symbols", {
  df <- data.frame( x = 1:4, test = rep(c(T,F), each = 2) )
  res <- filter(df, test)

  gdf <- group_by(df,x)
  res <- filter(gdf, test)

  h <- function(data){
    test2 <- c(T,T,F,F)
    filter(data,test2)
  }
  expect_equal(h(df), df[1:2,])

  f <- function(data, ...){
    one <- 1
    filter( data, test, x > one, ...)
  }
  g <- function(data, ...){
    four <- 4
    f( data, x < four, ...)
  }
  res <- g(df)
  expect_equal(res$x, 2L)
  expect_equal(res$test, TRUE)

  res <- g(gdf)
  expect_equal(res$x, 2L)
  expect_equal(res$test, TRUE)

})

test_that("filter handlers scalar results", {
  expect_equivalent( filter(mtcars, min(mpg)>0 ), mtcars )
  expect_equal( filter(group_by(mtcars,cyl), min(mpg)>0 ), group_by(mtcars,cyl) )
})

test_that("filter propagates attributes", {
  date.start <- ISOdate(2010, 01, 01, 0)
  test <- data.frame(Date = ISOdate(2010, 01, 01, 1:10))
  test2 <- test %>% filter(Date < ISOdate(2010, 01, 01, 5))
  expect_equal(test$Date[1:4], test2$Date)
})

test_that("filter fails on integer indices", {
  expect_error(filter(mtcars, 1:2))
  expect_error(filter(group_by(mtcars,cyl), 1:2))
})

test_that("filter discards NA", {
  temp <- data.frame(
    i = 1:5,
    x = c(NA, 1L, 1L, 0L, 0L)
  )
  res <- filter(temp, x == 1)
  expect_equal(nrow(res), 2L)
})

test_that("date class remains on filter (#273)",{
  x1 <- x2 <- data.frame(
    date = seq.Date(as.Date('2013-01-01'), by = "1 days", length.out = 2),
    var = c(5, 8)
  )
  x1.filter <- x1 %>% filter(as.Date(date) > as.Date('2013-01-01'))
  x2$date <- x2$date + 1
  x2.filter <- x2 %>% filter(as.Date(date) > as.Date('2013-01-01'))

  expect_equal(class(x1.filter$date), "Date")
  expect_equal(class(x2.filter$date), "Date")
})

test_that("filter handles $ correctly (#278)", {
  d1 <- tbl_df(data.frame(
    num1 = as.character(sample(1:10, 1000, T)),
    var1 = runif(1000),
    stringsAsFactors = FALSE))
  d2 <- data.frame(num1 = as.character(1:3), stringsAsFactors = FALSE)

  res1 <- d1 %>% filter(num1 %in% c("1", "2", "3"))
  res2 <- d1 %>% filter(num1 %in% d2$num1)
  expect_equal(res1, res2)
})

test_that( "filter returns the input data if no parameters are given", {
  expect_equivalent( filter(mtcars), mtcars )
})

test_that( "$ does not end call traversing. #502", {
  # Suppose some analysis options are set much earlier in the script
  analysis_opts <- list(min_outcome = .25)

  # Generate some dummy data
  d <- expand.grid(Subject = 1:3, TrialNo = 1:2, Time = 1:3) %>% tbl_df %>%
    arrange(Subject, TrialNo, Time) %>%
    mutate(Outcome = (1:18 %% c(5, 7, 11)) / 10)

  # Do some aggregation
  trial_outcomes <- d %>% group_by(Subject, TrialNo) %>%
    summarise(MeanOutcome = mean(Outcome))

  left  <- filter(trial_outcomes, MeanOutcome < analysis_opts$min_outcome)
  right <- filter(trial_outcomes, analysis_opts$min_outcome > MeanOutcome)

  expect_equal(left,right)

})

test_that( "GroupedDataFrame checks consistency of data (#606)", {
  df1 <- data.frame(
   group = factor(rep(c("C", "G"), 5)),
   value = 1:10)
  df1 <- df1 %>% group_by(group) #df1 is now tbl
  df2 <- data.frame(
     group = factor(rep("G", 10)),
     value = 11:20)
  df3 <- rbind(df1, df2) #df2 is data.frame

  expect_error( df3 %>% filter(group == "C"), "corrupt 'grouped_df', contains" )

})

test_that( "filter uses the white list (#566)", {
  datesDF <- read.csv(stringsAsFactors=FALSE, text="
X
2014-03-13 16:08:19
2014-03-13 16:16:23
2014-03-13 16:28:28
2014-03-13 16:28:54
")

  datesDF$X <- as.POSIXlt(datesDF$X)
  expect_error(filter(datesDF, X > as.POSIXlt("2014-03-13")), "has unsupported type")
})

test_that( "filter handles complex vectors (#436)", {
  d <- data.frame(x=1:10, y=1:10+2i)
  expect_equal(filter(d, x<4)$y, 1:3+2i)
  expect_equal(filter(d, Re(y)<4)$y, 1:3+2i)
})

test_that("%in% works as expected (#126)", {
  df <- data_frame( a = c("a", "b", "ab"), g = c(1,1,2) )

  res <- df %>% filter( a %in% letters )
  expect_equal(nrow(res), 2L)

  res <- df %>% group_by(g) %>% filter( a %in% letters )
  expect_equal(nrow(res), 2L)

})

test_that("row_number does not segfault with example from #781", {
  z <- data.frame(a=c(1,2,3))
  b <- "a"
  res <- z %>% filter(row_number(b) == 2)
  expect_equal( nrow(res), 0L )
})


# data.table --------------------------------------------------------------

test_that("filter succeeds even if column called V1 (#615)", {
  dt <- data.table::data.table(x = 1:10 ,V1 = 0)
  out <- dt %>% group_by(V1) %>% filter(x > 5)

  expect_equal(nrow(out), 5)
})

test_that("filter correctly handles empty data frames (#782)", {
  res <- data_frame() %>% filter(F)
  expect_equal( nrow(res), 0L )
  expect_true( is.null(names(res)) )
})

test_that("filter(.,TRUE,TRUE) works (#1210)", {
  df <- data.frame(x=1:5)
  res <- filter(df,TRUE,TRUE)
  expect_equal(res, df)
})
