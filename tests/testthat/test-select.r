context("Select")

df <- as.data.frame(as.list(setNames(1:26, letters)))
srcs <- temp_srcs(c("df", "dt", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)


test_that("two selects equivalent to one", {
  compare_tbls(tbls, function(tbl) tbl %>% select(l:s) %>% select(n:o),
    ref = select(df, n:o))
})

test_that("select does not lose grouping (#147)", {
  df <- tbl_df(data.frame(a = rep(1:4, 2), b = rep(1:4, each = 2), x = runif(8)))
  grouped <- df %>% group_by(a) %>% select(a, b, x)

  expect_equal(groups(grouped), list(quote(a)))
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

# Select variables -----------------------------------------------

test_that("select_vars prefix/suffix matching", {
  vars <- c("abc", "acd", "bbc", "bbd", "eee")
  expect_equal(
    select_vars(vars, starts_with("a")),
    c("abc" = "abc", "acd" = "acd")
  )
  expect_equal(
    select_vars(vars, ends_with("d")),
    c("acd" = "acd", "bbd" = "bbd")
  )
  expect_equal(select_vars(vars, contains("eee")), c("eee" = "eee"))
})

test_that("select_vars throws an error if an empty pattern is provided", {
  vars <- c("abc", "def", "ghi")
  expect_error(select_vars(vars, starts_with("")))
  expect_error(select_vars(vars, ends_with("")))
  expect_error(select_vars(vars, contains("")))
  expect_error(select_vars(vars, matches("")))
})

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


test_that("num_range selects numeric ranges", {
  vars <- c("x1", "x2", "x01", "x02", "x10", "x11")
  names(vars) <- vars

  expect_equal(select_vars(vars, num_range("x", 1:2)), vars[1:2])
  expect_equal(select_vars(vars, num_range("x", 1:2, width = 2)), vars[3:4])
  expect_equal(select_vars(vars, num_range("x", 10:11)), vars[5:6])
  expect_equal(select_vars(vars, num_range("x", 10:11, width = 2)), vars[5:6])
})

# Data table -------------------------------------------------------------------

test_that("select changes columns in copy of data table", {
  dt <- data.table::data.table(x = 1:4, y = letters[1:4])

  expect_equal(names(select(dt, x, z = y)), c("x", "z"))
  expect_equal(names(dt), c("x", "y"))


  gdt <- dt %>% group_by(x)
  expect_equal(names(select(gdt, x, z = y)), c("x", "z"))
  expect_equal(names(gdt), c("x", "y"))
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
  first <- tbls$sqlite %>% select(A = a)
  expect_equal(tbl_vars(first), "A")
  expect_equal(tbl_vars(first %>% select(A)), "A")
  expect_equal(tbl_vars(first %>% select(B = A)), "B")
})

test_that("select preserves grouping vars", {
  first <- tbls$sqlite %>% group_by(b) %>% select(a)
  expect_equal(tbl_vars(first), c("b", "a"))
})

test_that("rename handles grouped data (#640)", {
  res <- data_frame(a = 1, b = 2) %>% group_by(a) %>% rename(c = b)
  expect_equal(names(res), c("a", "c"))
})

# combine_vars ------------------------------------------------------------
# This is the low C++ function with on sees integer indices

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

# one_of ------------------------------------------------------------------

test_that("one_of gives useful error", {
  df <- data_frame(x = 1, y = 1)

  expect_error(select(df, one_of("z")), "Unknown variables: `z`")
  expect_error(select(df, one_of("x", "z")), "Unknown variables: `z`")
})

