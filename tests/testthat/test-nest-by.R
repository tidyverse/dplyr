test_that("returns expected type/data", {
  df <- data.frame(g = 1:2, x = 1:2, y = 1:2)
  out <- nest_by(df, g)

  expect_s3_class(out, "rowwise_df")
  expect_equal(group_vars(out), "g")
  expect_named(out, c("g", "data"))
})

test_that("can control key col", {
  df <- data.frame(g = 1:2, x = 1:2, y = 1:2)
  out <- nest_by(df, g, .key = "key")
  expect_named(out, c("g", "key"))
})

test_that("nest_by() inherits grouping", {
  df <- data.frame(g1 = 1:2, g2 = 1:2, x = 1:2, y = 1:2)

  expect_equal(
    df %>% group_by(g1) %>% nest_by() %>% group_vars(),
    "g1"
  )

  # And you can't have it both ways
  expect_error(df %>% group_by(g1) %>% nest_by("g2"), "re-group")
})

test_that("can control whether grouping data in list-col", {
  df <- data.frame(g = 1:2, x = 1:2, y = 1:2)
  out <- nest_by(df, g)
  expect_named(out$data[[1]], c("x", "y"))

  out <- nest_by(df, g, .keep = TRUE)
  expect_named(out$data[[1]], c("g", "x", "y"))
})
