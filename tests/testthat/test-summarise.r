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
srcs <- temp_srcs(c("df", "dt", "sqlite", "postgres", "JDBC_postgres"))
tbls <- temp_load(srcs, df)

test_that("summarise peels off a single layer of grouping", {
  for (i in seq_along(tbls)) {
    grouped <- group_by(tbls[[i]], x, y)
    summed <- summarise(grouped, n())

    expect_equal(unname(groups(summed)), list(quote(x)), info = names(tbls)[i])
  }
})

test_that("summarise can refer to variables that were just created (#138)", {
  res <- summarise(tbl_df(mtcars), cyl1 = mean(cyl), cyl2 = cyl1 + 1  )
  expect_equal(res$cyl2, mean(mtcars$cyl)+1)

  gmtcars <- group_by(tbl_df(mtcars), am)
  res <- summarise(gmtcars, cyl1 = mean(cyl), cyl2 = cyl1 + 1)
  res_direct <- summarise(gmtcars, cyl2 = mean(cyl) + 1)
  expect_equal(res$cyl2, res_direct$cyl2)
})

test_that("summarise refuses to modify grouping variable (#143)", {
  df <- data.frame( a = c(1,2,1,2), b = c(1,1,2,2), x = 1:4 )
  ds <- group_by(tbl_df(df), a, b)
  expect_error(
    summarise(ds, a = mean(x), a = b + 1),
    "cannot modify grouping variable"
  )
})

test_that("summarise gives proper errors (#153)", {
  df <- data.frame(x=as.numeric(sample(1e3, 1e4, TRUE)),
                   y=sample(1e4, 1e4, TRUE), z=runif(1e4))
  df <- tbl_df(df)
  df <- group_by(df, x, y)
  expect_error(summarise(df, diff(z)), "expecting a single value")
  expect_error(summarise(df, log(z)), "expecting a single value")
  expect_error(summarise(df, y[1:2]), "expecting a single value")
})

test_that("summarise handles constants (#153)", {
  df <- data.frame(a=1:4)
  today <- Sys.Date()
  now <- Sys.time()

  res <- summarise(tbl_df(df), int = 1L, num = 1.0,
    str = "foo", bool = TRUE, date = today, time = now)
  expect_equal(res$int, 1L)
  expect_equal(res$num, 1.0)
  expect_equal(res$str, "foo")
  expect_equal(res$bool, TRUE)
  expect_equal(res$date, today)
  expect_equal(res$time, now)

  res <- summarise(group_by(df,a), int = 1L, num = 1.0,
    str = "foo", bool = TRUE, date = today, time = now)
  expect_equal(res$int,  rep(1L,4))
  expect_equal(res$num,  rep(1.0,4))
  expect_equal(res$str,  rep("foo",4))
  expect_equal(res$bool, rep(TRUE,4))
  expect_equal(res$date, rep(today,4))
  expect_equal(res$time, rep(now,4))

})

test_that("summarise handles passing ...", {
  df <- data.frame( x = 1:4 )

  f <- function(...){
    x1 <- 1
    f1 <- function(x) x
    summarise(df, ..., x1 = f1(x1) )
  }
  g <- function(...){
    x2 <- 2
    f(x2 = x2, ...)
  }
  h <- function(before = "before", ..., after = "after"){
    g(before = before, ..., after = after )
  }

  res <- h( x3 = 3 )
  expect_equal(res$x1, 1 )
  expect_equal(res$x2, 2 )
  expect_equal(res$before, "before")
  expect_equal(res$after, "after")

  df <- tbl_df(df)
  res <- h( x3 = 3 )
  expect_equal(res$x1, 1 )
  expect_equal(res$x2, 2 )
  expect_equal(res$before, "before")
  expect_equal(res$after, "after")

  df <- group_by(df, x)
  res <- h( x3 = 3 )
  expect_equal(res$x1, rep(1, 4) )
  expect_equal(res$x2, rep(2, 4) )
  expect_equal(res$before, rep("before",4))
  expect_equal(res$after, rep("after",4))

})

test_that( "summarise propagate attributes (#194)", {
  df <- group_by(data.frame(
    b = rep(1:2,2),
    f = Sys.Date() + 1:4,
    g = Sys.time() + 1:4,
    stringsAsFactors = FALSE
  ), b)

  min_ <- min
  res <- summarise( df,
    min_f  = min(f),
    max_f  = max(f),
    min_g  = min(g),
    max_g  = max(g),
    min__f = min_(f),
    min__g = min_(g)
  )

  expect_equal(class(res$min_f) , "Date" )
  expect_equal(class(res$max_f) , "Date" )
  expect_equal(class(res$min__f), "Date" )

  expect_equal(class(res$min_g) , c("POSIXct", "POSIXt" ))
  expect_equal(class(res$max_g) , c("POSIXct", "POSIXt" ))
  expect_equal(class(res$min__g), c("POSIXct", "POSIXt" ))

})

test_that("summarise fails on missing variables", {
  expect_error(summarise(mtcars, a = mean(notthear)) )
})

test_that("n() does not accept arguments",{
  expect_error(summarise(group_by(mtcars, cyl), n(hp)), "does not take arguments")
})

test_that("hybrid nests correctly", {
  res <- group_by(mtcars, cyl) %>% summarise(a = if(n()>10) 1 else 2 )
  expect_equal(res$a, c(1,2,1))

  res <- mtcars %>% summarise(a = if(n()>10) 1 else 2 )
  expect_equal(res$a, 1)
})

test_that("hybrid min and max propagate attributes (#246)", {
  x <- data.frame(id=c(rep("a",2), rep("b",2)),
                date=as.POSIXct(c("2014-01-13", "2014-01-14",
                                  "2014-01-15", "2014-01-16"),
                                tz="GMT"))
  y <- x %>% group_by(id) %>% summarise(max_date=max(date), min_date=min(date))

  expect_true("tzone" %in% names(attributes(y$min_date)))
  expect_true("tzone" %in% names(attributes(y$max_date)))
})

test_that("summarise can use newly created variable more than once", {
  df <- data.frame(id=c(1,1,2,2,3,3), a=1:6) %>% group_by(id)
  for( i in 1:10){
    res <- summarise(df, biggest=max(a), smallest=min(a), diff1=biggest-smallest, diff2=smallest-biggest)
    expect_equal( res$diff1, -res$diff2)
  }
})

test_that("summarise creates an empty data frame when no parameters are used", {
  res <- summarise(mtcars)
  expect_equal(res,data.frame())
})

test_that("integer overflow (#304)",{
  groups <- rep(c('A', 'B'), each = 3)
  values <- rep(1e9,  6)
  dat <- data.frame(groups,
                X1 = as.integer(values),
                X2 = values)
  # now group and summarise
  res <- group_by(dat, groups) %>%
               summarise(sum_integer = sum(X1),
                         sum_numeric = sum(X2))
  expect_true( all(is.na(res$sum_integer)) )
  expect_equal( res$sum_numeric, rep(3e9, 2L) )
})

test_that("summarise checks outputs (#300)", {
  expect_error( summarise(mtcars, mpg, cyl) )
  expect_error( summarise(mtcars, mpg + cyl) )
})

test_that("comment attribute is white listed (#346)",{
  test <- data.frame(A = c(1,1,0,0), B = c(2,2,3,3))
  comment(test$B) <- "2nd Var"
  res <- group_by(test, A)
  expect_equal(comment(res$B), "2nd Var" )
})

test_that("AsIs class is white listed (#453)",{
  test <- data.frame(A = c(1,1,0,0), B = I(c(2,2,3,3)))
  res <- group_by(test, B)
  expect_equal(res$B, test$B )
})

test_that("names attribute is not retained (#357)", {
  df <- data.frame(x=c(1:3), y=letters[1:3])
  df <- group_by(df, y)
  m <- df %>% summarise(
    a=length(x),
    b=quantile(x, 0.5)
  )
  expect_equal(m$b, c(1,2,3))
  expect_null(names(m$b))
})

test_that("na.rm is supported (#168)", {
  df <- data.frame( x = c(1:5, NA, 7:10), y = rep(1:2, each = 5 ), z = c(rnorm(5), NA, rnorm(4) ) )
  res <- df %>% group_by(y) %>%
    summarise(
      mean_x = mean(x, na.rm = FALSE), mean_z = mean(z, na.rm = FALSE),
      min_x = min(x, na.rm = FALSE), min_z = min(z, na.rm = FALSE)
    )
  expect_equal( res$mean_x[1], 3 )
  expect_true( is.na( res$mean_x[2] ) )
  expect_equal( res$mean_z[1], mean(df$z[1:5]) )
  expect_true( is.na(res$mean_z[2]) )

  expect_equal( res$min_x[1], 1 )
  expect_true( is.na( res$min_x[2] ) )
  expect_equal( res$min_z[1], min(df$z[1:5]) )
  expect_true( is.na(res$min_z[2]) )

  res <- df %>% group_by(y) %>%
    summarise(
      mean_x = mean(x, na.rm = TRUE), mean_z = mean(z, na.rm = TRUE),
      min_x = min(x, na.rm = TRUE), min_z = min(z, na.rm = TRUE)
      )
  expect_equal( res$mean_x[1], 3 )
  expect_equal( res$mean_x[2], 8.5 )
  expect_equal( res$mean_z[1], mean(df$z[1:5]) )
  expect_equal( res$mean_z[2], mean(df$z[7:10]) )

  expect_equal( res$min_x[1], 1 )
  expect_equal( res$min_x[2], 7 )
  expect_equal( res$min_z[1], min(df$z[1:5]) )
  expect_equal( res$min_z[2], min(df$z[7:10]) )

})

test_that( "summarise hybrid functions can use summarized variables", {
  df <- data.frame( x = c(1:5, NA, 7:10), y = rep(1:2, each = 5 ) ) %>% group_by(y)
  res <- summarise( df, x = mean(x), min = min(x), max = max(x), mean = mean(x), var = var(x) )
  expect_identical( res$x, res$min )
  expect_identical( res$x, res$max )
  expect_identical( res$x, res$mean )
  expect_identical( res$var, rep(NA_real_, 2) )
})
