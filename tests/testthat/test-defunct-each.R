test_that("generate informative errors", {
  expect_snapshot(error = TRUE, {
    summarise_each()
    summarise_each_()
    mutate_each()
    mutate_each_()
    summarize_each()
    summarize_each_()
  })
})
