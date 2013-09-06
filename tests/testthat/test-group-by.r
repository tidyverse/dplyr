context("Group by")

df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
tbls <- clone_tbls(df)

test_that("group_by adds to additional groups", {
  for (tbl in tbls) {
    grp_x <- group_by(tbl, x)
    grp_xy <- group_by(grp_x, y)
    
    expect_equal(unname(groups(grp_xy)), list(quote(x), quote(y)))
  }
})

test_that("collect and compute preserve grouping", {
  g <- group_by(tbls$sqlite, x, y)
  
  expect_equal(groups(compute(g)), groups(g))
  expect_equal(groups(collect(g)), groups(g))
})

test_that("collapse drops groups", {
  g <- group_by(tbls$sqlite, x, y)
  
  expect_equal(groups(collapse(g)), NULL)  
})

