context("Group sizes")

# Data for the first three test_that groups below
df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
tbls <- test_load(df)

test_that("ungrouped data has 1 group, with group size = nrow()", {
  for (tbl in tbls) {
    expect_equal(n_groups(tbl), 1L)
    expect_equal(group_size(tbl), 30)
  }
})

test_that("rowwise data has one group for each group", {
  rw <- rowwise(df)
  expect_equal(n_groups(rw), 30)
  expect_equal(group_size(rw), rep(1, 30))
})

test_that("group_size correct for grouped data", {
  for (tbl in tbls) {
    grp <- group_by(tbl, x)
    expect_equal(n_groups(grp), 3L)
    expect_equal(group_size(grp), rep(10, 3))
  }
})


# For following tests, add an extra level that's not present in data
df$x = factor(df$x, levels=1:4)
tbls <- test_load(df)

test_that("n_groups drops zero-length groups", {
  for (tbl in tbls) {
    grp <- group_by(tbl, x)
    expect_equal(n_groups(grp), 3, info=class(tbl)[1])
  }
})

test_that("summarise drops zero-length groups", {
  for (tbl in tbls) {
    res <- as_data_frame(summarise(group_by(tbl, x), n=n(), mn=mean(y)))
    expect_equal(nrow(res), 3, info=class(tbl)[1])
    expect_equal(tail(res$n, n=1), 10, info=class(tbl)[1])
    expect_false(is.nan(tail(res$mn, n=1)), info=class(tbl)[1])
  }
})
