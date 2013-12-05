context("Group by")

df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))

srcs <- temp_srcs(c("df", "dt", "cpp", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)

test_that("group_by adds to additional groups", {
  add_groups1 <- function(tbl) groups(group_by(tbl, x, y))
  add_groups2 <- function(tbl) groups(group_by(group_by(tbl, x), y))
  
  expect_equal(add_groups1(tbls$df), list(x = quote(x), y = quote(y)))
  expect_equal(add_groups2(tbls$df), list(x = quote(x), y = quote(y)))

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
    
    expect_equal(groups(inner_join(g, g)), groups(g))
    expect_equal(groups(left_join(g, g)), groups(g))
    expect_equal(groups(semi_join(g, g)), groups(g))
    expect_equal(groups(anti_join(g, g)), groups(g))    
  }
})

test_that("constructors drops groups", {
  dt <- lahman_src("dt") %.% tbl("Batting") %.% group_by(playerID)
  df <- lahman_src("df") %.% tbl("Batting") %.% group_by(playerID)
  
  expect_equal(groups(tbl_dt(dt)), NULL)
  expect_equal(groups(tbl_df(df)), NULL)
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
srcs <- temp_srcs(c("df", "dt", "cpp"))
var_tbls <- temp_load(srcs, df_var)

group_by_ <- function(x, vars) {
  call <- as.call(c(quote(group_by), quote(x), lapply(vars, as.symbol)))
  eval(call)
}

test_that("local group_by preserves variable types", {
  for(var in names(df_var)) {
    expected <- data.frame(unique(df_var[[var]]), n = 1L, 
      stringsAsFactors = FALSE)
    names(expected)[1] <- var
    
    for(tbl in names(var_tbls)) {
      grouped <- group_by_(var_tbls[[tbl]], var)
      summarised <- as.data.frame(summarise(grouped, n = n()))
      
      expect_equal(summarised, expected, 
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

