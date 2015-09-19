context("Group by")

df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))

srcs <- temp_srcs(c("df", "dt", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)

test_that("group_by with add = TRUE adds groups", {
  add_groups1 <- function(tbl) groups(group_by(tbl, x, y, add = TRUE))
  add_groups2 <- function(tbl) groups(group_by(group_by(tbl, x, add = TRUE), y,
    add = TRUE))

  expect_equal(add_groups1(tbls$df), list(quote(x), quote(y)))
  expect_equal(add_groups2(tbls$df), list(quote(x), quote(y)))

  expect_equal(add_groups1(tbls$dt), list(quote(x), quote(y)))
  expect_equal(add_groups2(tbls$dt), list(quote(x), quote(y)))

  expect_equal(add_groups1(tbls$sqlite), list(quote(x), quote(y)))
  expect_equal(add_groups2(tbls$sqlite), list(quote(x), quote(y)))
})

test_that("collect, collapse and compute preserve grouping", {
  g <- group_by(tbls$sqlite, x, y)

  expect_equal(groups(compute(g)), groups(g))
  expect_equal(groups(collapse(g)), groups(g))
  expect_equal(groups(collect(g)), groups(g))
})

test_that("joins preserve grouping", {
  for (tbl in tbls) {
    g <- group_by(tbl, x)

    expect_equal(groups(inner_join(g, g, by = c("x", "y"))), groups(g))
    expect_equal(groups(left_join(g, g, by = c("x", "y"))), groups(g))
    expect_equal(groups(semi_join(g, g, by = c("x", "y"))), groups(g))
    expect_equal(groups(anti_join(g, g, by = c("x", "y"))), groups(g))
  }
})

test_that("constructors drops groups", {
  dt <- lahman_dt() %>% tbl("Batting") %>% group_by(playerID)
  df <- lahman_df() %>% tbl("Batting") %>% group_by(playerID)

  expect_equal(groups(tbl_dt(dt)), NULL)
  expect_equal(groups(tbl_df(df)), NULL)
})

test_that("grouping by constant adds column (#410)", {
  grouped <- group_by(mtcars, "cyl") %>% summarise(foo = n())
  expect_equal(names(grouped), c('"cyl"', "foo"))
  expect_equal(nrow(grouped), 1L)
})

# Test full range of variable types --------------------------------------------

df_var <- data.frame(
  l = c(T, F),
  i = 1:2,
  d = Sys.Date() + 1:2,
  f = factor(letters[1:2]),
  num = 1:2 + 0.5,
  t = Sys.time() + 1:2,
  c = letters[1:2],
  stringsAsFactors = FALSE
)
srcs <- temp_srcs(c("df", "dt"))
var_tbls <- temp_load(srcs, df_var)

test_that("local group_by preserves variable types", {
  for(var in names(df_var)) {
    expected <- data.frame(unique(df_var[[var]]), n = 1L,
      stringsAsFactors = FALSE)
    names(expected)[1] <- var

    for(tbl in names(var_tbls)) {
      grouped <- group_by_(var_tbls[[tbl]], var)
      summarised <- summarise(grouped, n = n())
      expect_true(all.equal(summarised, expected),
        label = paste0("summarised_", tbl, "_", var))
    }
  }
})

test_that("mutate does not loose variables (#144)",{
  df <- tbl_df(data.frame(a = rep(1:4, 2), b = rep(1:4, each = 2), x = runif(8)))
  by_ab <- group_by(df, a, b)
  by_a  <- summarise( by_ab, x = sum(x))
  by_a_quartile <- group_by(by_a, quartile = ntile(x, 4))

  expect_equal(names(by_a_quartile), c("a", "b", "x", "quartile" ))
})

test_that("group_by uses shallow copy", {
  m1 <- group_by(mtcars, cyl)
  expect_true(is.null(groups(mtcars)))

  expect_equal(dfloc(mtcars), dfloc(m1))
})

test_that("FactorVisitor handles NA. #183", {
  g <- group_by(MASS::survey, M.I)
  expect_equal(g$M.I, MASS::survey$M.I)
})

test_that("group_by orders by groups. #242", {
  df <- data.frame(a = sample(1:10, 100, replace = TRUE)) %>% group_by(a)
  expect_equal( attr(df, "labels")$a, 1:10 )

  df <- data.frame(a = sample(letters[1:10], 100, replace = TRUE), stringsAsFactors = FALSE) %>% group_by(a)
  expect_equal(attr(df, "labels")$a, letters[1:10] )

  df <- data.frame(a = sample(sqrt(1:10), 100, replace = TRUE)) %>% group_by(a)
  expect_equal(attr(df, "labels")$a, sqrt(1:10))

})

test_that("group_by uses the white list", {
  df <- data.frame( times = 1:5 )
  df$times <- as.POSIXlt( seq.Date( Sys.Date(), length.out = 5, by = "day" ) )
  expect_error(group_by(df, times))
})

test_that("group_by fails when lists are used as grouping variables (#276)",{
  df <- data.frame(x = 1:3)
  df$y <- list(1:2, 1:3, 1:4)
  expect_error(group_by(df,y))
})

# Data tables ------------------------------------------------------------------

test_that("original data table not modified by grouping", {
  dt <- data.table::data.table(x = 5:1)
  dt2 <- group_by(dt, x)
  dt2$y <- 1:5

  expect_equal(dt$x, 5:1)
  expect_equal(dt$y, NULL)
})

test_that("select(group_by(.)) implicitely adds grouping variables (#170)", {
  res <- mtcars %>% group_by(vs) %>% select(mpg)
  expect_equal(names(res), c("vs", "mpg"))

  res <- mtcars %>% tbl_dt() %>% group_by(vs) %>% select(mpg)
  expect_equal(names(res), c("vs", "mpg"))
})

test_that("grouped_df errors on empty vars (#398)",{
  m <- mtcars %>% group_by(cyl)
  attr(m, "vars") <- NULL
  attr(m, "indices") <- NULL
  expect_error( m %>% do(mpg = mean(.$mpg)) )
})

test_that("group_by only creates one group for NA (#401)", {
  x <- as.numeric(c(NA,NA,NA,10:1,10:1))
  w <- c(20,30,40,1:10,1:10)*10

  n_distinct(x) # 11 OK
  res <- data.frame(x=x,w=w) %>% group_by(x) %>% summarise(n=n())
  expect_equal(nrow(res), 11L)
})

test_that("data.table invalid .selfref issue (#475)", {
  dt <- data.table::data.table(x=1:5, y=6:10)
  expect_that((dt %>% group_by(x))[, z := 2L], not(gives_warning()))
  dt <- data.table::data.table(x=1:5, y=6:10)
  expect_that((dt %>% group_by(x) %>% summarise(z = y^2))[, foo := 1L], not(gives_warning()))
})

test_that("there can be 0 groups (#486)", {
  data <- data.frame(a = numeric(0), g = character(0)) %>% group_by(g)
  expect_equal(length(data$a), 0L)
  expect_equal(length(data$g), 0L)
  expect_equal(attr(data, "group_sizes"), integer(0))
})

test_that("group_by works with zero-row data frames (#486)", {
  dfg <- group_by(data.frame(a = numeric(0), b = numeric(0), g = character(0)), g)
  expect_equal(dim(dfg), c(0, 3))
  expect_equal(groups(dfg), list(quote(g)))
  expect_equal(group_size(dfg), integer(0))

  x <- summarise(dfg, n = n())
  expect_equal(dim(x), c(0, 2))
  expect_equal(groups(x), NULL)

  x <- mutate(dfg, c = b + 1)
  expect_equal(dim(x), c(0, 4))
  expect_equal(groups(x), list(quote(g)))
  expect_equal(group_size(x), integer(0))

  x <- filter(dfg, a == 100)
  expect_equal(dim(x), c(0, 3))
  expect_equal(groups(x), list(quote(g)))
  expect_equal(group_size(x), integer(0))

  x <- arrange(dfg, a, g)
  expect_equal(dim(x), c(0, 3))
  expect_equal(groups(x), list(quote(g)))
  expect_equal(group_size(x), integer(0))

  x <- select(dfg, a)  # Only select 'a' column; should result in 'g' and 'a'
  expect_equal(dim(x), c(0, 2))
  expect_equal(groups(x), list(quote(g)))
  expect_equal(group_size(x), integer(0))
})

test_that("grouped_df requires a list of symbols (#665)", {
  features <- list("feat1", "feat2", "feat3")
  expect_error( grouped_df(data.frame(feat1=1, feat2=2, feat3=3), features) )
})

test_that("group_by gives meaningful message with unknow column (#716)",{
  expect_error( group_by(iris, wrong_name_of_variable), "unknown column" )
})

test_that("[ on grouped_df preserves grouping if subset includes grouping vars", {
  df <- data_frame(x = 1:5, ` ` = 6:10)
  by_x <- df %>% group_by(x)
  expect_equal(by_x %>% groups(), by_x %>% `[`(1:2) %>% groups)

  # non-syntactic name
  by_ns <- df %>% group_by(` `)
  expect_equal(by_ns %>% groups(), by_ns %>% `[`(1:2) %>% groups)
})


test_that("[ on grouped_df drops grouping if subset doesn't include grouping vars", {
  by_cyl <- mtcars %>% group_by(cyl)
  no_cyl <- by_cyl %>% `[`(c(1, 3))

  expect_equal(groups(no_cyl), NULL)
  expect_is(no_cyl, "tbl_df")
})

test_that("group_by works after arrange (#959)",{
  df  <- data_frame(Log= c(1,2,1,2,1,2), Time = c(10,1,3,0,15,11))
  res <- df %>%
     arrange(Time) %>%
     group_by(Log) %>%
     mutate(Diff = Time - lag(Time))
  expect_true( all(is.na( res$Diff[ c(1,3) ] )))
  expect_equal( res$Diff[ c(2,4,5,6) ], c(1,7,10,5) )
})

test_that("group_by keeps attributes", {
  d <- data.frame( x = structure( 1:10, foo = "bar" ) )
  gd <- group_by(d)
  expect_equal( attr(gd$x, "foo"), "bar")
})

test_that("ungroup.rowwise_df gives a tbl_df (#936)", {
  res <- tbl_df(mtcars) %>% rowwise %>% ungroup %>% class
  expect_equal( res, c("tbl_df", "data.frame"))
})
