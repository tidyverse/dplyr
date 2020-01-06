context("group_data")

test_that("group_rows works for 3 most important subclasses (#3489)", {
  df <- data.frame(x=c(1,1,2,2))
  expect_equivalent(group_rows(df), list(1:4))
  expect_equivalent(group_rows(group_by(df,x)), list(1:2, 3:4))
  expect_equivalent(group_rows(rowwise(df)), as.list(1:4))
})

test_that("group_data() returns a tidy tibble (#3489)", {
  df <- tibble(x = c(1,1,2,2))

  expect_is(group_data(df), "tbl_df")
  expect_equal(names(group_data(df)), ".rows")

  expect_equivalent(group_rows(df), list(1:4))

  gd <- group_by(df,x)
  expect_is(group_data(gd), "tbl_df")
  expect_equal(names(group_data(gd)), c("x", ".rows"))

  expect_equivalent(
    group_rows(gd),
    list(1:2, 3:4)
  )
  expect_equivalent(
    group_keys(gd),
    tibble(x = c(1,2))
  )

  rd <- rowwise(df)
  expect_is(group_data(rd), "tbl_df")
  expect_equal(names(group_data(rd)), c(".rows"))

  expect_equivalent(group_rows(rd), as.list(1:4))
})

test_that("group_rows and group_data work with 0 rows data frames (#3489)", {
  df <- tibble(x=integer())
  expect_identical(group_rows(df), list_of(integer()))
  expect_identical(group_rows(rowwise(df)), list_of(.ptype = integer()))
  expect_identical(group_rows(group_by(df, x)), list_of(.ptype = integer()))

  expect_identical(group_data(df), tibble(".rows" := list_of(integer())))
  expect_identical(group_data(rowwise(df)), tibble(".rows" := list_of(.ptype = integer())))
  expect_identical(
    group_data(group_by(df, x)),
    tibble::new_tibble(list(x = integer(), .rows = list_of(.ptype = integer())), .drop = TRUE, nrow = 0L)
  )
})

# Errors ------------------------------------------------------------------

test_that("group_data() give meaningful errors", {
  df1 <- group_by(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
  groups <- attr(df1, "groups")
  groups[[2]] <- 1:2
  attr(df1, "groups") <- groups

  df2 <- group_by(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
  groups <- attr(df2, "groups")
  names(groups) <- c("g", "not.rows")
  attr(df2, "groups") <- groups

  df3 <- df2
  attr(df3, "groups") <- tibble()

  df4 <- df3
  attr(df4, "groups") <- NA

  df5 <- tibble(x = 1:4, g = rep(1:2, each = 2))
  attr(df5, "vars") <- "g"
  attr(df5, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")

  verify_output(test_path("test-group_data.txt"), {
    "# Invalid `groups` attribute"
    group_data(df1)
    group_data(df2)
    group_data(df3)
    group_data(df4)

    "# Older style grouped_df"
    validate_grouped_df(df5)
  })
})
