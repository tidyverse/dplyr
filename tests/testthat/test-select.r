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

test_that("no inputs selects all vars", {
  vars <- c("a", "b")
  expect_equal(select_vars(vars), c("a" = "a", "b" = "b"))
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
  dt <- data.table(x = 1:4, y = letters[1:4])

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
