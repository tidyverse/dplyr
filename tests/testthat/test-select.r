context("Select")

df <- as.data.frame(as.list(setNames(1:26, letters)))
tbls <- test_load(df)

test_that("two selects equivalent to one", {
  compare_tbls(tbls, function(tbl) tbl %>% select(l:s) %>% select(n:o),
    ref = select(df, n:o))
})

test_that("select does not lose grouping (#147)", {
  df <- tbl_df(data.frame(a = rep(1:4, 2), b = rep(1:4, each = 2), x = runif(8)))
  grouped <- df %>% group_by(a) %>% select(a, b, x)

  expect_equal(groups(grouped), list(quote(a)))
})

test_that("grouping variables preserved with a message (#1511)", {
  df <- data_frame(g = 1:3, x = 3:1) %>% group_by(g)

  expect_message(res <- select(df, x), "Adding missing grouping variables")
  expect_named(res, c("g", "x"))
})

test_that("non-syntactic grouping variable is preserved (#1138)", {
  df <- data_frame(`a b` = 1L) %>% group_by(`a b`) %>% select()
  expect_named(df, "a b")
})

test_that("select doesn't fail if some names missing", {
  df1 <- data.frame(x = 1:10, y = 1:10, z = 1:10)
  df2 <- setNames(df1, c("x", "y", ""))
  # df3 <- setNames(df1, c("x", "", ""))

  expect_equal(select(df1, x), data.frame(x = 1:10))
  expect_equal(select(df2, x), data.frame(x = 1:10))
  # expect_equal(select(df3, x), data.frame(x = 1:10))
})

# Empty selects -------------------------------------------------

test_that("select with no args returns nothing", {
  empty <- select(mtcars)
  expect_equal(ncol(empty), 0)
  expect_equal(nrow(empty), 32)
})

test_that("select excluding all vars returns nothing", {
  expect_equal(dim(select(mtcars, -(mpg:carb))), c(32, 0))
  expect_equal(dim(select(mtcars, starts_with("x"))), c(32, 0))
  expect_equal(dim(select(mtcars, -matches("."))), c(32, 0))
})

test_that("negating empty match returns everything", {
  df <- data.frame(x = 1:3, y = 3:1)
  expect_equal(select(df, -starts_with("xyz")), df)
})

# Select variables -----------------------------------------------

test_that("select_vars can rename variables", {
  vars <- c("a", "b")
  expect_equal(select_vars(vars, b = a, a = b), c("b" = "a", "a" = "b"))
})

test_that("last rename wins", {
  vars <- c("a", "b")

  expect_equal(select_vars(vars, b = a, c = a), c("c" = "a"))
})

test_that("negative index removes values", {
  vars <- letters[1:3]

  expect_equal(select_vars(vars, -c), c("a" = "a", "b" = "b"))
  expect_equal(select_vars(vars, a:c, -c), c("a" = "a", "b" = "b"))
  expect_equal(select_vars(vars, a, b, c, -c), c("a" = "a", "b" = "b"))
  expect_equal(select_vars(vars, -c, a, b), c("a" = "a", "b" = "b"))
})

test_that("select can be before group_by (#309)",{
  df <- data.frame(id=c(1,1,2,2,2,3,3,4,4,5), year=c(2013,2013,2012,2013,2013,2013,2012,2012,2013,2013), var1=rnorm(10))
  dfagg <- df %>%
    group_by(id, year) %>%
    select(id, year, var1) %>%
    summarise(var1=mean(var1))
  expect_equal(names(dfagg), c("id", "year", "var1"))
  expect_equal(attr(dfagg, "vars" ), list(quote(id)))

})

# Database ---------------------------------------------------------------------

test_that("select renames variables (#317)", {
  skip_if_no_sqlite()

  first <- tbls$sqlite %>% select(A = a)
  expect_equal(tbl_vars(first), "A")
  expect_equal(tbl_vars(first %>% select(A)), "A")
  expect_equal(tbl_vars(first %>% select(B = A)), "B")
})

test_that("select preserves grouping vars", {
  skip_if_no_sqlite()

  first <- tbls$sqlite %>% group_by(b) %>% select(a)
  expect_equal(tbl_vars(first), c("b", "a"))
})

test_that("rename handles grouped data (#640)", {
  res <- data_frame(a = 1, b = 2) %>% group_by(a) %>% rename(c = b)
  expect_equal(names(res), c("a", "c"))
})

# combine_vars ------------------------------------------------------------
# This is the low C++ function which works on integer indices

test_that("empty index gives empty output", {
  vars <- combine_vars(letters, list())
  expect_equal(length(vars), 0)

  vars <- combine_vars(letters, list(numeric()))
  expect_equal(length(vars), 0)
})

test_that("positive indexes kept", {
  expect_equal(combine_vars(letters, list(1)), c(a = 1))
  expect_equal(combine_vars(letters, list(1, 26)), c(a = 1, z = 26))
  expect_equal(combine_vars(letters, list(c(1, 26))), c(a = 1, z = 26))
})

test_that("indexes returned in order they appear", {
  expect_equal(combine_vars(letters, list(26, 1)), c(z = 26, a = 1))
})


test_that("negative index in first position includes all others", {
  vars <- combine_vars(letters[1:3], list(-1))
  expect_equal(vars, c(b = 2, c = 3))
})

test_that("named inputs rename outputs", {
  expect_equal(combine_vars(letters[1:3], list(d = 1)), c(d = 1))
  expect_equal(combine_vars(letters[1:3], list(c(d = 1))), c(d = 1))
})

test_that("if multiple names, last kept", {
  expect_equal(combine_vars(letters[1:3], list(d = 1, e = 1)), c(e = 1))
  expect_equal(combine_vars(letters[1:3], list(c(d = 1, e = 1))), c(e = 1))
})

test_that("if one name for multiple vars, use integer index", {
  expect_equal(combine_vars(letters[1:3], list(x = 1:3)), c(x1 = 1, x2 = 2, x3 = 3))
})

test_that("invalid inputs raise error", {
  expect_error(combine_vars(names(mtcars), list(0)), "positive or negative")
  expect_error(combine_vars(names(mtcars), list(c(-1, 1))), "positive or negative")
  expect_error(combine_vars(names(mtcars), list(12)), "must be between")
})

test_that("select succeeds in presence of raw columns (#1803)", {
  df <- data_frame(a = 1:3, b = as.raw(1:3))
  expect_identical(select(df, a), df["a"])
  expect_identical(select(df, b), df["b"])
  expect_identical(select(df, -b), df["a"])
})

test_that("select_if can use predicate", {
  expect_identical(iris %>% select_if(is.factor), iris["Species"])
})

test_that("select_if fails with databases", {
  expect_error(memdb_frame(x = 1) %>% select_if(is.numeric) %>% collect())
})

test_that("select_if keeps grouping cols", {
  expect_silent(df <- iris %>% group_by(Species) %>% select_if(is.numeric))
  expect_equal(df, tbl_df(iris[c(5, 1:4)]))
})
