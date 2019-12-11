context("new_grouped_df")

test_that("new grouped_df checks that `group_data` has a `.rows` column (#3837)", {
  tbl <- tibble(x = 1:10)
  expect_error(new_grouped_df(tbl, tibble(other = list(1:2))))
})

test_that("new_grouped_df can create alternative grouping structures (#3837)", {
  tbl <- new_grouped_df(
    tibble(x = rnorm(10)),
    groups = tibble(".rows" := replicate(5, sample(1:10, replace = TRUE), simplify = FALSE))
  )
  res <- summarise(tbl, x = mean(x))
  expect_equal(nrow(res), 5L)
})

test_that("validate_grouped_df (#3837)", {
  df <- new_grouped_df(
    tibble(x = 1:10),
    groups = tibble(".rows" := list(1:5, -1L))
  )
  expect_error(validate_grouped_df(df, check_bounds = TRUE), "out of bounds indices", class = "dplyr_grouped_df_corrupt")

  attr(df, "groups")$.rows <- list(11L)
  expect_error(validate_grouped_df(df, check_bounds = TRUE), "out of bounds indices", class = "dplyr_grouped_df_corrupt")

  attr(df, "groups")$.rows <- list(0L)
  expect_error(validate_grouped_df(df, check_bounds = TRUE), "out of bounds indices", class = "dplyr_grouped_df_corrupt")

  attr(df, "groups")$.rows <- list(1)
  expect_error(validate_grouped_df(df), "`.rows` column is not a list of one-based integer vectors", class = "dplyr_grouped_df_corrupt")

  attr(df, "groups") <- tibble()
  expect_error(validate_grouped_df(df), "The `groups` attribute is not a data frame with its last column called `.rows`", class = "dplyr_grouped_df_corrupt")

  attr(df, "groups") <- NULL
  expect_error(validate_grouped_df(df), "The `groups` attribute is not a data frame with its last column called `.rows`", class = "dplyr_grouped_df_corrupt")
})

test_that("new_grouped_df does not have rownames (#4173)", {
  tbl <- new_grouped_df(
    tibble(x = rnorm(10)),
    groups = tibble(".rows" := replicate(5, sample(1:10, replace = TRUE), simplify = FALSE))
  )
  expect_false(tibble::has_rownames(tbl))
})
