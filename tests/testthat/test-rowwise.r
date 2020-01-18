test_that("rowwise status preserved by major verbs", {
  rf <- rowwise(tibble(x = 1:5, y = 5:1), "x")

  out <- arrange(rf, y)
  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "x")

  out <- filter(rf, x < 3)
  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "x")

  out <- mutate(rf, x = x + 1)
  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "x")

  out <- rename(rf, X = x)
  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "X")

  out <- select(rf, "x")
  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "x")

  out <- slice(rf, c(1, 1))
  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "x")

  out <- summarise(rf, z = mean(x, y))
  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "x")
})

test_that("rowwise nature preserved by subsetting ops", {
  rf <- rowwise(tibble(x = 1:5, y = 1:5), "x")

  out <- rf[1]
  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "x")

  out[, "z"] <- 5:1
  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "x")

  names(out) <- toupper(names(out))
  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "X")
})

test_that("except when it should be removed", {
  rf <- rowwise(tibble(x = 1:5, y = 1:5), "x")
  expect_equal(out <- rf[, 1, drop = TRUE], rf$x)
})

test_that("rowwise has decent print method", {
  verify_output(test_path("test-rowwise-print.txt"), {
    rf <- rowwise(tibble(x = 1:5), "x")
    rf
  })
})
