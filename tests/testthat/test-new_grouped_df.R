context("new_grouped_df")

test_that("new grouped_df checks that `group_data` has a `.rows` column (#3837)", {
  tbl <- tibble(x = 1:10)
  expect_error(new_grouped_df(tbl, tibble(other = list(1:2))))
})

test_that("new_grouped_df can create alternative grouping structures (#3837)", {
  tbl <- new_grouped_df(
    tibble(x = rnorm(10)),
    groups = tibble(.rows = replicate(5, sample(1:10, replace = TRUE), simplify = FALSE))
  )
  res <- summarise(tbl, x = mean(x))
  expect_equal(nrow(res), 5L)
})
