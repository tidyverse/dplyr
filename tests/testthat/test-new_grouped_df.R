context("new_grouped_df")

test_that("new grouped_df checks that `group_data` has a `.rows` column (#3837)", {
  tbl <- tibble(x = 1:10)
  expect_error(new_grouped_df(tbl, tibble()), "`group_data` is a corrupt grouped structure")
  expect_error(new_grouped_df(tbl, tibble(other = list(1:2))), "`.data` is a corrupt grouped_df, the `\"groups\"` attribute must have a list column named `.rows` as last column")
})

test_that("new grouped_df checks bounds (#3837)", {
  tbl <- tibble(x = 1:10)
  expect_error(new_grouped_df(tbl, tibble(.rows = list(0L))), "`group_data` is a corrupt group structure, index in group 1 out of bounds")
  expect_error(new_grouped_df(tbl, tibble(.rows = list(1:3, -1L))), "`group_data` is a corrupt group structure, index in group 2 out of bounds")
  expect_error(new_grouped_df(tbl, tibble(.rows = list(1:3, 1:3, 11L))), "`group_data` is a corrupt group structure, index in group 3 out of bounds")
})
