context("hybrid")

test_that("hybrid evaluation environment is cleaned up (#2358)", {
  # Can't use pipe here, f and g should have top-level parent.env()
  df <- data_frame(x = 1)
  df <- mutate(df, f = list(function(){}))
  df <- mutate(df, g = list(quo(.)))
  df <- mutate(df, h = list(~.))

  expect_environments_clean(df$f[[1]])
  expect_environments_clean(df$g[[1]])
  expect_environments_clean(df$h[[1]])

  # Avoids "Empty test" message
  expect_true(TRUE)
})

test_that("n() and n_distinct() work", {
  check_hybrid_result(
    n(), a = 1:5,
    expected = 5L, test_eval = FALSE
  )
  check_not_hybrid_result(
    list(1:n()), a = 1:5,
    expected = list(1:5), test_eval = FALSE
  )

  check_hybrid_result(
    n_distinct(a), a = 1:5,
    expected = 5L
  )
  check_hybrid_result(
    n_distinct(a), a = rep(1L, 3),
    expected = 1L
  )
})
