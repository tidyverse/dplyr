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

  # Except for summarise
  out <- summarise(rf, z = mean(x, y))
  expect_s3_class(out, "grouped_df")
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
  rf <- rowwise(tibble(x = 1:5), "x")
  expect_snapshot(rf)
})

test_that("rowwise captures group_vars", {
  df <- group_by(tibble(g = 1:2, x = 1:2), g)
  rw <- rowwise(df)
  expect_equal(group_vars(rw), "g")

  # but can't regroup
  expect_error(rowwise(df, x), "Can't re-group")
})

test_that("can re-rowwise", {
  rf1 <- rowwise(tibble(x = 1:5, y = 1:5), "x")
  rf2 <- rowwise(rf1, y)
  expect_equal(group_vars(rf2), "y")
})

test_that("new_rowwise_df() does not require `group_data=`", {
  df <- new_rowwise_df(data.frame(x = 1:2))
  expect_s3_class(df, "rowwise_df")
  expect_equal(attr(df, "groups"), tibble(".rows" := vctrs::list_of(1L, 2L)))
})

test_that("new_rowwise_df() can add class and attributes (#5918)", {
  df <- new_rowwise_df(tibble(x = 1:4), tibble(), class = "custom_rowwise_df", a = "b")
  expect_s3_class(df, "custom_rowwise_df")
  expect_equal(attr(df, "a"), "b")
})

test_that("validate_rowwise_df() gives useful errors", {
  expect_snapshot(error = TRUE, {
    df1 <- rowwise(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
    groups <- attr(df1, "groups")
    groups[[2]] <- 4:1
    attr(df1, "groups") <- groups
    validate_rowwise_df(df1)

    df2 <- rowwise(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
    groups <- attr(df2, "groups")
    names(groups) <- c("g", "not.rows")
    attr(df2, "groups") <- groups
    validate_rowwise_df(df2)

    df3 <- df2
    attr(df3, "groups") <- tibble()
    validate_rowwise_df(df3)

    df4 <- df3
    attr(df4, "groups") <- NA
    validate_rowwise_df(df4)

    df7 <- rowwise(tibble(x = 1:10))
    attr(df7, "groups")$.rows <- 11:20
    validate_rowwise_df(df7)

    df8 <- rowwise(tibble(x = 1:10))
    attr(df8, "groups")$.rows <- 1:8
    validate_rowwise_df(df8)

    df10 <- df7
    attr(df10, "groups") <- tibble()
    validate_rowwise_df(df10)

    df11 <- df7
    attr(df11, "groups") <- NULL
    validate_rowwise_df(df11)

    new_rowwise_df(
      tibble(x = 1:10),
      tibble(".rows" := list(1:5, -1L))
    )

    new_rowwise_df(
      tibble(x = 1:10),
      1:10
    )
  })
})
