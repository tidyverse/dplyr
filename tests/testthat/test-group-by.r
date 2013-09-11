context("Group by")

df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
tbls <- clone_tbls(df)

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

test_that("collect and compute preserve grouping", {
  g <- group_by(tbls$sqlite, x, y)
  
  expect_equal(groups(compute(g)), groups(g))
  expect_equal(groups(collect(g)), groups(g))
})

test_that("join and semi join preserve grouping", {
  g <- group_by(tbls$sqlite, x)
  
  expect_equal(groups(join(g, g)), groups(g))
  expect_equal(groups(semi_join(g, g)), groups(g))
})


test_that("collapse drops groups", {
  g <- group_by(tbls$sqlite, x, y)
  
  expect_equal(groups(collapse(g)), NULL)  
})

