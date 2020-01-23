test_that("group_cols() selects grouping variables", {
  df <- tibble(x = 1:3, y = 1:3)
  gf <- group_by(df, x)

  expect_equal(df %>% select(group_cols()), df[integer()])
  expect_message(
    expect_equal(gf %>% select(group_cols()), gf["x"]),
    NA
  )
})

test_that("group_cols(vars) is deprecated", {
  expect_warning(out <- group_cols("a"), "deprecated")
  expect_equal(out, integer())
})

test_that("group_cols() finds groups in scoped helpers", {
  gf <- group_by(tibble(x = 1, y = 2), x)
  expect_warning(out <- select_at(gf, vars(group_cols())))
  expect_named(out, "x")
})
