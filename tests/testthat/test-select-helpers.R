test_that("group_cols() selects grouping variables", {
  df <- tibble(x = 1:3, y = 1:3)
  gf <- group_by(df, x)

  expect_equal(df %>% select(group_cols()), df[integer()])
  expect_message(
    expect_equal(gf %>% select(group_cols()), gf["x"]),
    NA
  )
})


