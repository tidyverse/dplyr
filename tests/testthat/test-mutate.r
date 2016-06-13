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
tbls <- test_load(df)

test_that("two mutates equivalent to one", {
  compare_tbls(tbls, function(tbl) tbl %>% mutate(x2 = x * 2, y4 = y * 4))
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
})

test_that("mutate errors when results are not compatible accross groups (#299)",{
  d <- data.frame(x = rep(1:5, each = 3))
  expect_error(mutate(group_by(d,x),val = ifelse(x < 3, "foo", 2)))
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

test_that("mutate allows list columns (#555)", {
  df  <- data.frame(x = c("a;b", "c;d;e"), stringsAsFactors = FALSE)
  res <- mutate( df, pieces = strsplit(x, ";"))
  expect_equal(res$pieces, list(c("a", "b"), c("c", "d", "e")))
})

test_that("hybrid evaluation goes deep enough (#554)", {
  res1 <- iris %>% mutate(test = 1 == 2 | row_number() < 10)
  res2 <- iris %>% mutate(test = row_number() < 10 | 1 == 2)
  expect_equal(res1,res2)
})

test_that("hybrid does not segfault when given non existing variable (#569)", {
  expect_error( mtcars %>% summarise(first(mp)), "could not find variable" )
})

test_that("namespace extraction works in hybrid (#412)", {
  df <- data.frame(x = 1:2)

  expect_equal(
    mutate(df, y = base::mean(x)),
    mutate(df, y = mean(x))
  )
  expect_equal(
    mutate(df, y = stats::IQR(x)),
    mutate(df, y = IQR(x))
  )
})

test_that("hybrid not get in the way of order_by (#169)", {
  df <- data_frame(x = 10:1, y = 1:10)
  res <- mutate(df, z = order_by(x, cumsum(y)))
  expect_equal(res$z, rev(cumsum(10:1)))
})

test_that("mutate supports difftime objects (#390)", {
  df <- data_frame(
    grp =   c(1, 1,  2, 2),
    val =   c(1, 3,  4, 6),
    date1 = c(rep(Sys.Date() - 10, 2), rep(Sys.Date() - 20, 2)),
    date2 = Sys.Date() + c(1,2,1,2),
    diffdate = difftime(date2, date1, unit = "days")
  )

  res <- df %>% group_by(grp) %>%
    mutate(mean_val = mean(val), mean_diffdate = mean(diffdate) )
  expect_is(res$mean_diffdate, "difftime")
  expect_equal( as.numeric(res$mean_diffdate), c(11.5,11.5,21.5,21.5))

  res <- df %>% group_by(grp) %>% summarise(dt = mean(diffdate))
  expect_is( res$dt, "difftime" )
  expect_equal( as.numeric(res$dt), c(11.5,21.5) )
})

test_that("mutate works on zero-row grouped data frame (#596)", {
  dat <- data.frame(a = numeric(0), b = character(0))
  res <- dat %>% group_by(b) %>% mutate(a2 = a*2)
  expect_is(res$a2, "numeric")
  expect_is(res, "grouped_df")
  expect_equal(res$a2, numeric(0))
  expect_equal(attr(res, "indices"), list())
  expect_equal(attr(res, "vars"), list( quote(b) ))
  expect_equal(attr(res, "group_sizes"), integer(0))
  expect_equal(attr(res, "biggest_group_size"), 0L)
})

test_that("Non-ascii column names in version 0.3 are not duplicated (#636)", {
  df  <- data_frame(a = "1", b = "2")
  names(df) <- c("a", enc2native("\u4e2d"))

  res <- df %>% mutate_each(funs(as.numeric)) %>% names
  expect_equal(res, names(df))
})

test_that("nested hybrid functions do the right thing (#637)", {
  res <- mtcars %>% mutate(mean(1))
  expect_true( all( res[["mean(1)"]] == 1L ) )
})

test_that("mutate handles using and gathering complex data (#436)", {
  d <- data_frame(x=1:10, y=1:10+2i)
  res <- mutate(d, real=Re(y), imag=Im(y), z=2*y, constant=2+2i)
  expect_equal(names(res), c("x", "y", "real", "imag", "z", "constant"))
  expect_equal(res$real, Re(d$y))
  expect_equal(res$imag, Im(d$y))
  expect_equal(res$z, d$y * 2)
  expect_true( all(res$constant == 2+2i) )
})

test_that("mutate forbids POSIXlt results (#670)", {
  expect_error(
    data.frame(time='2014/01/01 10:10:10') %>% mutate(time=as.POSIXlt(time)),
    "does not support"
  )

  expect_error(
    data.frame(time='2014/01/01 10:10:10', a=2) %>% group_by(a) %>% mutate(time=as.POSIXlt(time)),
    "does not support"
  )

})

test_that("constant factor can be handled by mutate (#715)",{
  d <- data_frame(x=1:2) %>% mutate(y=factor("A"))
  expect_true( is.factor(d$y) )
  expect_equal( d$y, factor( c("A", "A") ) )
})

test_that("row_number handles empty data frames (#762)", {
  df <- data.frame(a = numeric(0))
  res <- df %>% mutate(
    row_number_0 = row_number(), row_number_a =  row_number(a), ntile = ntile(a, 2),
    min_rank = min_rank(a), percent_rank = percent_rank(a),
    dense_rank = dense_rank(a), cume_dist = cume_dist(a)
  )
  expect_equal( names(res), c("a", "row_number_0", "row_number_a", "ntile", "min_rank", "percent_rank", "dense_rank", "cume_dist" ) )
  expect_equal( nrow(res), 0L )
})

test_that("no utf8 invasion (#722)", {
  skip_on_cran()

  source("utf-8.R", local = TRUE)
})

test_that("mutate works on empty data frames (#1142)", {
  df <- data.frame()
  res <- df %>% mutate
  expect_equal( nrow(res), 0L )
  expect_equal( length(res), 0L )

  res <- df %>% mutate(x = numeric())
  expect_equal( names(res), "x")
  expect_equal( nrow(res), 0L )
  expect_equal( length(res), 1L)
})

test_that("mutate handles 0 rows rowwise #1300",{
  a <- data.frame(x= 1)
  b <- data.frame(y = character(), stringsAsFactors = F)

  g <- function(y){1}
  f <- function() { b %>% rowwise() %>% mutate(z = g(y))}

  res <- f()
  expect_equal( nrow(res), 0L )

  expect_error(a %>% mutate(b = f()), "wrong result size" )
  expect_error(a %>% rowwise() %>% mutate(b = f()), "incompatible size")
})

test_that("regression test for #637", {
  res <- mtcars %>% mutate(xx = mean(1))
  expect_true( all(res$xx == 1))

  res <- mtcars %>% mutate(xx = sum(mean(mpg)))
  expect_true( all( res$xx == sum(mean(mtcars$mpg))))
})

test_that("mutate.rowwise handles factors (#886)", {
  res <- data.frame(processed=c("foo", "bar")) %>%
    rowwise() %>%
    mutate(processed_trafo=paste("test", processed))
  expect_equal( res$processed_trafo, c("test foo", "test bar"))
})

test_that("setting first column to NULL with mutate works (#1329)", {
    df <- data.frame(x = 1:10, y = 1:10)
    expect_equal( mutate(df, x=NULL), select(df,-x) )
    expect_equal( mutate(df, y=NULL), select(df,-y) )

    gdf <- group_by(df, y)
    expect_equal( select(gdf, -x), mutate(gdf, x = NULL) )
})

test_that("mutate handles the all NA case (#958)", {
  x <- rep(c("Bob", "Jane"), each = 36)
  y <- rep(rep(c("A", "B", "C"), each = 12), 2)
  day <- rep(rep(1:12, 3), 2)
  values <- rep(rep(c(10, 11, 30, 12, 13, 14, 15, 16, 17, 18, 19, 20), 3), 2)

  df <- data.frame(x = x, y = y, day = day, values = values)
  df$values[1:12] <- NA

  res <- df %>%
    group_by(x, y) %>%
    mutate(max.sum = day[which.max(values)[1]]) %>%
    mutate(adjusted_values = ifelse(day < max.sum, 30, values))
  expect_true( all(is.na( res$adjusted_values[1:12] )))
})

test_that("rowwie mutate gives expected results (#1381)", {
  f  <- function( x ) ifelse( x < 2, NA_real_, x )
  res <- data_frame( x = 1:3 ) %>% rowwise() %>% mutate( y = f(x) )
  expect_equal( res$y, c(NA,2,3) )
})

test_that("mutate handles factors (#1414)", {
  d <- data_frame( g = c(1,1,1,2,2,3,3), f = c("a", "b", "a", "a", "a", "b", "b" ) )
  res <- d %>% group_by(g) %>% mutate( f2 = factor(f) )
  expect_equal( as.character(res$f2), res$f)
})

test_that("mutate recognizes global #1469", {
  vs <- 4
  res <- mtcars %>% mutate(a = global(vs))
  expect_true( all(res$a == 4) )
  expect_error( mtcars %>% mutate(global("vs")), "global only handles symbols" )
  res <- mtcars %>% mutate(a = global(vs) + 1)
  expect_true( all(res$a == 5) )
  expect_error( mtcars %>% mutate(global("vs") + 1), "global only handles symbols" )
  res <- mtcars %>% mutate(a = 1+global(vs) )
  expect_true( all(res$a == 5) )
  expect_error( mtcars %>% mutate(1 + global("vs")), "global only handles symbols" )

  res <- mtcars %>% group_by(cyl) %>% mutate(a = global(vs))
  expect_true( all(res$a == 4) )
  expect_error( mtcars %>% group_by(cyl) %>% mutate(a = global("vs")), "global only handles symbols" )
  res <- mtcars %>% group_by(cyl) %>% mutate(a = global(vs)+1)
  expect_true( all(res$a == 5) )
  expect_error( mtcars %>% group_by(cyl) %>% mutate(a = global("vs") + 1), "global only handles symbols" )

  res <- mtcars %>% group_by(cyl) %>% mutate(a = 1+global(vs))
  expect_true( all(res$a == 5) )
  expect_error( mtcars %>% group_by(cyl) %>% mutate(a = 1 + global("vs")), "global only handles symbols" )
})

test_that("mutate handles results from one group with all NA values (#1463) ", {
  df <- data_frame( x = c(1, 2), y = c(1, NA))
  res <- df %>% group_by(x) %>% mutate( z = ifelse(y>1, 1, 2) )
  expect_true( is.na(res$z[2]) )
  expect_is( res$z, "numeric")
})

test_that("rowwise mutate handles the NA special case (#1448)", {
  res <- data.frame(k = c(-1, 1, 1)) %>%
    rowwise() %>%
    mutate(l = ifelse(k > 0, 1, NA))
  expect_is(res$l, "numeric")
  expect_true( is.na(res$l[1]) )
  expect_true( !anyNA(res$l[-1]) )

  res <- data.frame(k = rnorm(10)) %>%
    rowwise() %>%
    mutate(l = ifelse(k > 0, 1L, NA_integer_))
  expect_true( all(is.na(res$l[res$k <= 0]) ) )
  expect_true( !any(is.na(res$l[res$k > 0]) ) )
})

test_that("mutate disambiguates NA and NaN (#1448)", {
  Pass <- data.frame(P2 = c(0,3,2), F2 = c(0,2,0), id = 1:3)
  res <- Pass %>% group_by(id) %>% mutate(pass2 = P2/(P2 + F2))
  expect_true( is.nan(res$pass2[1]) )

  res <- Pass %>% rowwise %>% mutate(pass2 = P2/(P2 + F2))
  expect_true( is.nan(res$pass2[1]) )

  Pass <- data_frame(
    P1 = c(2L, 0L, 10L,8L, 9L),
    F1 = c(0L, 2L, 0L, 4L,3L),
    P2 = c(0L, 3L, 2L, 2L, 2L),
    F2 = c(0L, 2L, 0L, 1L,1L),
    id = c(1,2,4,4,5)
  )

  res <- Pass %>%
     group_by(id) %>%
       dplyr::mutate(pass_rate = (P1 + P2) / (P1 + P2 + F1 + F2) * 100,
              pass_rate1 = P1 / (P1 + F1) * 100,
              pass_rate2 = P2 / (P2 + F2) * 100)
  expect_true( is.nan(res$pass_rate2[1]) )
})

test_that("hybrid evaluator leaves formulas untouched (#1447)", {
  d <- data_frame(g = 1:2, training = list(mtcars, mtcars * 2))
  mpg <- data.frame(x=1:10, y=1:10)
  res <- d %>%
    group_by(g) %>%
    mutate(lm_result = list(lm(mpg ~ wt, data = training[[1]])))
  expect_is( res$lm_result, "list" )
  expect_is( res$lm_result[[1]], "lm" )
  expect_is( res$lm_result[[2]], "lm" )
})

test_that( "lead/lag inside mutate handles expressions as value for default (#1411) ", {
  df <- data_frame(x = 1:3)
  res <- mutate(df, leadn = lead(x, default = x[1]), lagn = lag(x, default = x[1]) )
  expect_equal( res$leadn, lead(df$x, default = df$x[1]) )
  expect_equal( res$lagn, lag(df$x, default = df$x[1]) )

  res <- mutate(df, leadn = lead(x, default = c(1)), lagn = lag(x, default = c(1)))
  expect_equal( res$leadn, lead(df$x, default = 1) )
  expect_equal( res$lagn, lag(df$x, default = 1) )
})

test_that("mutate understands column. #1012", {
  ir1 <- mutate( iris, Sepal = Sepal.Length * Sepal.Width )
  ir2 <- mutate( iris, Sepal = column("Sepal.Length") * column("Sepal.Width") )
  expect_equal(ir1, ir2)

  ir1 <- mutate( group_by(iris, Species), Sepal = Sepal.Length * Sepal.Width )
  ir2 <- mutate( group_by(iris, Species), Sepal = column("Sepal.Length") * column("Sepal.Width") )
  expect_equal(ir1, ir2)

  ir <- iris %>% mutate( a = column("Species") )
  expect_equal( ir$a, ir$Species)

  ir <- iris %>% group_by(Species) %>% mutate( a = column("Species") )
  expect_equal( ir$a, ir$Species)
})

test_that("grouped mutate does not drop grouping attributes (#1020)", {
  d <- data.frame(subject=c('Jack','Jill'),id=c(2,1)) %>% group_by(subject)
  a1 <- names(attributes(d))
  a2 <- names(attributes(d %>% mutate(foo=1)))
  expect_equal( setdiff(a1, a2), character(0) )
})

test_that("grouped mutate errors on incompatible column type (#1641)", {
  df <- data.frame(ID = rep(1:5, each = 3), x = 1:15) %>% group_by(ID)
  expect_error( mutate(df, foo = mean), 'Unsupported type CLOSXP for column "foo"')
})

test_that("lead/lag works on more complex expressions (#1588)", {
  df <- data_frame(x = rep(1:5,2), g = rep(1:2, each = 5) ) %>% group_by(g)
  res <- df %>% mutate( y = lead(x > 3) )
  expect_equal(res$y, rep(lead(1:5 > 3), 2) )
})

test_that("Adding a Column of NA to a Grouped Table gives expected results (#1645)", {
  dataset <- data_frame(A = 1:10, B = 10:1, group = factor(sample(LETTERS[25:26], 10, TRUE)))
  res <- dataset %>% group_by(group) %>% mutate(prediction = factor(NA))
  expect_true( all(is.na(res$prediction) ) )
  expect_is( res$prediction, "factor")
  expect_equal( levels(res$prediction), character() )
})

test_that("Deep copies are performed when needed (#1463)", {
  res <- data.frame(prob = c(F,T)) %>%
    rowwise %>%
    mutate(model = list(x=prob) )
  expect_equal(unlist(res$model), c(FALSE,TRUE))

  res <- data.frame(x=1:4, g=c(1,1,1,2)) %>%
    group_by(g) %>%
    mutate(model = list(y=x) )
  expect_equal(res$model[[1]], 1:3)
  expect_equal(res$model[[4]], 4)
})

test_that( "ntile falls back to R (#1750)", {
  res <- mutate( iris, a = ntile("Sepal.Length", 3))
  expect_equal( res$a, rep(1, 150))
})

test_that("mutate fails gracefully on raw columns (#1803)", {
  df <- data_frame(a = 1:3, b = as.raw(1:3))
  expect_error( mutate(df, a = 1), 'Unsupported type RAWSXP for column "b"' )
  expect_error( mutate(df, b = 1), 'Unsupported type RAWSXP for column "b"' )
  expect_error( mutate(df, c = 1), 'Unsupported type RAWSXP for column "b"' )
})
