test_that("ungrouping by variable generates error", {
  df <- tibble(x = 1)
  expect_error(ungroup(df, x), class = "rlib_error_dots_nonempty")
})
