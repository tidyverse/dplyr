expect_groups <- function(df, groups, info = NULL) {
  if (length(groups) == 0L) {
    expect_equal(groups(df), list(), info = info)
    expect_identical(group_vars(df), character(), info = info)
  } else {
    expect_identical(groups(df), lapply(enc2native(groups), as.name), info = info)
    expect_identical(group_vars(df), groups, info = info)
  }
}

expect_no_groups <- function(df) {
  expect_groups(df, character())
}
