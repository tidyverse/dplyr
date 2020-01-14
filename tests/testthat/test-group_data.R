
# group_data --------------------------------------------------------------

test_that("group_data(<data.frame>) returns a data frame", {
  df <- data.frame(x = 1:3)
  gd <- group_data(df)

  expect_s3_class(gd, "data.frame", exact = TRUE)
  expect_equal(gd$.rows, list_of(1:3))
})

test_that("group_data(<tbl_df>) returns a tibble", {
  df <- tibble(x = 1:3)
  gd <- group_data(df)

  expect_s3_class(gd, "tbl_df")
  expect_equal(gd, tibble(".rows" := list_of(1:3)))
})

test_that("group_data(<grouped_df>) returns a tibble", {
  df <- tibble(x = c(1, 1, 2))
  gf <- group_by(df, x)
  gd <- group_data(gf)

  expect_s3_class(gd, "tbl_df")
  expect_equivalent(gd, tibble(x = c(1, 2), ".rows" := list_of(1:2, 3L)))
})

test_that("group_data(<rowwise) returns a tibble", {
  df <- tibble(x = 1:3)
  rf <- rowwise(df)
  gd <- group_data(rf)

  expect_s3_class(gd, "tbl_df")
  expect_equal(gd, tibble(".rows" := list_of(1, 2, 3)))
})

# group_rows() and group_keys() -------------------------------------------

test_that("group_rows() and group_keys() partition group_data()", {
  df <- data.frame(x = 1:2, y = 1:2)
  gf <- group_by(df, x, y)
  gd <- group_data(gf)

  expect_equivalent(group_keys(gf), gd[1:2]) # .drop attribute
  expect_equal(group_rows(gf), gd[[3]])
})

test_that("group_keys(...) is deprecated", {
  df <- tibble(x = 1, y = 2)

  expect_warning(out <- df %>% group_keys(x), "deprecated")
  expect_equal(out, tibble(x = 1))
})

# group_indices() ---------------------------------------------------------

test_that("no arg group_indices() is deprecated", {
  df <- tibble(x = 1)
  expect_warning(out <- summarise(df, id = group_indices()), "deprecated")
  expect_equal(out, tibble(id = 1))
})

test_that("group_indices(...) is deprecated", {
  df <- tibble(x = 1, y = 2)
  expect_warning(out <- df %>% group_indices(x), "deprecated")
  expect_equal(out, 1)
})

test_that("group_indices() returns expected values", {
  df <- tibble(x = c("b", "a", "b"))
  gf <- group_by(df, x)

  expect_equal(group_indices(df), c(1, 1, 1))
  expect_equal(group_indices(gf), c(2, 1, 2))
})

# group_size --------------------------------------------------------------

test_that("ungrouped data has 1 group, with group size = nrow()", {
  df <- tibble(x = rep(1:3, each = 10), y = rep(1:6, each = 5))

  expect_equal(n_groups(df), 1L)
  expect_equal(group_size(df), 30)
})

test_that("rowwise data has one group for each group", {
  rw <- rowwise(mtcars)
  expect_equal(n_groups(rw), 32)
  expect_equal(group_size(rw), rep(1, 32))
})

test_that("group_size correct for grouped data", {
  df <- tibble(x = rep(1:3, each = 10), y = rep(1:6, each = 5)) %>% group_by(x)
  expect_equal(n_groups(df), 3L)
  expect_equal(group_size(df), rep(10, 3))
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

  df6 <- new_grouped_df(
    tibble(x = 1:10),
    groups = tibble(".rows" := list(1:5, -1L))
  )
  df7 <- df6
  attr(df7, "groups")$.rows <- list(11L)

  df8 <- df6
  attr(df8, "groups")$.rows <- list(0L)

  df9 <- df6
  attr(df9, "groups")$.rows <- list(1)

  df10 <- df6
  attr(df10, "groups") <- tibble()

  df11 <- df6
  attr(df11, "groups") <- NULL

  verify_output(test_path("test-group_data-errors.txt"), {
    "# Invalid `groups` attribute"
    group_data(df1)
    group_data(df2)
    group_data(df3)
    group_data(df4)

    "# Older style grouped_df"
    validate_grouped_df(df5)

    "# new_grouped_df()"
    new_grouped_df(
      tibble(x = 1:10),
      tibble(other = list(1:2))
    )

    "# validate_grouped_df()"
    validate_grouped_df(df6, check_bounds = TRUE)
    validate_grouped_df(df7, check_bounds = TRUE)
    validate_grouped_df(df8, check_bounds = TRUE)
    validate_grouped_df(df9)
    validate_grouped_df(df10)
    validate_grouped_df(df11)
  })
})
