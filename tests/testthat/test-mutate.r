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
srcs <- temp_srcs(c("df", "dt", "sqlite", "postgres", "JDBC_postgres"))
tbls <- temp_load(srcs, df)

test_that("two mutates equivalent to one", {
  compare_tbls(tbls, function(tbl) tbl %>% mutate(x2 = x * 2, y4 = y * 4), convert = T)
})

test_that("mutate can refer to variables that were just created (#140)", {
  res <- mutate(tbl_df(mtcars), cyl1 = cyl + 1, cyl2 = cyl1 + 1)
  expect_equal(res$cyl2, mtcars$cyl+2)

  gmtcars <- group_by(tbl_df(mtcars), am)
  res <- mutate(gmtcars, cyl1 = cyl + 1, cyl2 = cyl1 + 1)
  res_direct <- mutate(gmtcars, cyl2 = cyl + 2)
  expect_equal(res$cyl2, res_direct$cyl2)
})

test_that("mutate handles logical result (#141)", {
  x <- data.frame(x = 1:10, g = rep(c(1, 2), each = 5))
  res <- tbl_df(x) %>% group_by(g) %>% mutate(r = x > mean(x))
  expect_equal(res$r, rep(c(FALSE,FALSE,FALSE,TRUE,TRUE), 2))
})

test_that("mutate can rename variables (#137)", {
  res <- mutate(tbl_df(mtcars), cyl2 = cyl)
  expect_equal(res$cyl2, mtcars$cyl)

  res <- mutate(group_by(tbl_df(mtcars), am) , cyl2 = cyl)
  expect_equal(res$cyl2, res$cyl)
})

test_that("mutate refuses to modify grouping vars (#143)", {
  expect_error(mutate(group_by(tbl_df(mtcars), am) , am = am + 2),
    "cannot modify grouping variable")
})

test_that("mutate handles constants (#152)", {
  res <- mutate(tbl_df(mtcars), zz = 1)
  expect_equal(res$zz, rep(1, nrow(mtcars)))
})

test_that("mutate fails with wrong result size (#152)", {
  df <- group_by(data.frame(x = c(2, 2, 3, 3)), x)
  expect_equal(mutate(df, y = 1:2)$y, rep(1:2,2))
  expect_error(mutate( mtcars, zz = 1:2 ), "wrong result size" )

  df <- group_by(data.frame(x = c(2, 2, 3, 3, 3)), x)
  expect_error(mutate(df, y = 1:2))
})

test_that("mutate refuses to use symbols not from the data", {
  y <- 1:6
  df <- group_by(data.frame(x = c(1, 2, 2, 3, 3, 3)), x)
  expect_error(mutate( df, z = y ))
})

test_that("mutate recycles results of length 1", {
  df <- data.frame(x = c(2, 2, 3, 3))
  expect_equal(mutate(tbl_df(df), z = length(x) )$z, rep(4,4))
  expect_equal(mutate(group_by(df,x), z = length(x) )$z, rep(2,4))

  int  <- 1L
  str  <- "foo"
  num  <- 1
  bool <- TRUE

  res <- mutate(group_by(df,x),
    int = int, str = str, num = num, bool = bool)
  expect_equal(res$int , rep(int ,4))
  expect_equal(res$str , rep(str ,4))
  expect_equal(res$num , rep(num ,4))
  expect_equal(res$bool, rep(bool,4))
})


test_that("mutate handles out of data variables", {
  today <- Sys.Date()
  now <- Sys.time()
  df  <- data.frame(x = c(2, 2, 3, 3))
  gdf <- group_by(df,x)

  int  <- c(1L,2L)
  str  <- c("foo", "bar")
  num  <- c(1,2)
  bool <- c(TRUE,FALSE)
  dat  <- rep(today,2)
  tim  <- rep(now,2)

  res <- mutate(gdf, int = int, str = str, num = num, bool = bool,
    dat = dat, tim = tim)
  expect_equal(res$int , rep(int ,2))
  expect_equal(res$str , rep(str ,2))
  expect_equal(res$num , rep(num ,2))
  expect_equal(res$bool, rep(bool,2))
  expect_equal(res$dat, rep(dat,2))
  expect_equal(res$tim, rep(tim,2))

  int <- 1:6
  expect_error(mutate(gdf, int = int))
  expect_error(mutate(tbl_df(df), int = int))

  int  <- 1:4
  str  <- rep(c("foo", "bar"), 2 )
  num  <- c(1,2,3,4)
  bool <- c(TRUE,FALSE,FALSE,TRUE)
  dat  <- rep(today,4)
  tim  <- rep(now,4)

  res <- mutate(tbl_df(df), int = int, str = str, num = num, bool = bool, tim = tim, dat=dat)
  expect_equal(res$int , int  )
  expect_equal(res$str , str  )
  expect_equal(res$num , num  )
  expect_equal(res$bool, bool )
  expect_equal(res$dat , dat  )
  expect_equal(res$tim , tim  )
})

test_that("mutate handles passing ...", {
  df <- data.frame( x = 1:4 )

  f <- function(...){
    x1 <- 1
    f1 <- function(x) x
    mutate(df, ..., x1 = f1(x1) )
  }
  g <- function(...){
    x2 <- 2
    f(x2 = x2, ...)
  }
  h <- function(before = "before", ..., after = "after"){
    g(before = before, ..., after = after )
  }

  res <- h( x3 = 3 )
  expect_equal(res$x1, rep(1,4) )
  expect_equal(res$x2, rep(2,4) )
  expect_equal(res$before, rep("before", 4))
  expect_equal(res$after, rep("after", 4))

  df <- tbl_df(df)
  res <- h( x3 = 3 )
  expect_equal(res$x1, rep(1,4) )
  expect_equal(res$x2, rep(2,4) )
  expect_equal(res$before, rep("before", 4))
  expect_equal(res$after, rep("after", 4))

  df <- group_by(df, x)
  res <- h( x3 = 3 )
  expect_equal(res$x1, rep(1,4) )
  expect_equal(res$x2, rep(2,4) )
  expect_equal(res$before, rep("before", 4))
  expect_equal(res$after, rep("after", 4))

})

test_that("mutate fails on unsupported column type", {
  df <- data.frame(created = c("2014/1/1", "2014/1/2", "2014/1/2"))
  expect_error(mutate(df, date = strptime(created, "%Y/%m/%d")) )

  df <- data.frame(created = c("2014/1/1", "2014/1/2", "2014/1/2"), g = c(1,1,2) )
  expect_error(mutate(group_by(df,g), date = strptime(created, "%Y/%m/%d")) )
})

test_that("mutate modifies same column repeatedly (#243)", {
  df <- data.frame(x = 1)
  expect_equal(mutate(df, x = x + 1, x = x + 1)$x, 3)

  dt <- data.table(x = 1)
  expect_equal(mutate(dt, x = x + 1, x = x + 1)$x, 3)
})

test_that("mutate errors when results are not compatible accross groups (#299)",{
  d <- data.frame(x = rep(1:5, each = 3))
  expect_error(mutate(group_by(d,x),val = ifelse(x < 3, NA, 2)))
})

test_that("assignments are forbidden (#315)", {
   expect_error(mutate(mtcars, cyl2 = { x <- cyl^2; -x } ))
})

test_that("hybrid evaluator uses correct environment (#403)", {
  func1 <- function() {
    func2 <- function(x) floor(x)
    mutate(mtcars, xx = func2(mpg / sum(mpg)))
  }
  res <- func1()
  expect_equal(res$xx, rep(0,nrow(res)) )
})

test_that("mutate remove variables with = NULL syntax (#462)", {
  data <- mtcars %>% mutate(cyl = NULL)
  expect_false( "cyl" %in% names(data) )
  
  data <- mtcars %>% group_by(disp) %>% mutate(cyl = NULL)
  expect_false( "cyl" %in% names(data) )
})

test_that("mutate(rowwise_df) makes a rowwise_df (#463)", {
  one_mod <- data.frame(grp = "a", x = runif(5,0,1)) %>%
    tbl_df %>%
    mutate(y = rnorm(x,x*2,1)) %>%
    group_by(grp) %>%
    do(mod = lm(y~x,data = .)) 
  
  out <- one_mod %>%
    mutate(rsq = summary(mod)$r.squared) %>%
    mutate(aic = AIC(mod))
                
  expect_is(out, "rowwise_df")
  expect_equal(nrow(out), 1L) 
  expect_is(out$mod, "list")
  expect_is(out$mod[[1L]], "lm" )  
})

