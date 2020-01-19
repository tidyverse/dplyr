test_that("condense always returns a rowwise", {
  df <- tibble(x = 1)
  gf <- group_by(df, x)
  rf <- rowwise(df)

  expect_s3_class(condense(df, y = 1), "rowwise_df")
  expect_s3_class(condense(gf, y = 1), "rowwise_df")
  expect_s3_class(condense(rf, y = 1), "rowwise_df")
})
