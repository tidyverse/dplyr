context("n_distinct")

test_that("count_distinct gives the correct results on iris", {
  expect_equal(
    sapply(iris, n_distinct),
    sapply(iris, function(.) length(unique(.)))
  )
})

df_var <- data.frame(
  l = c(T, F, F),
  i = c(1, 1, 2),
  d = Sys.Date() + c(1, 1, 2),
  f = factor(letters[c(1, 1, 2)]),
  n = c(1, 1, 2) + 0.5,
  t = Sys.time() + c(1, 1, 2),
  c = letters[c(1, 1, 2)],
  stringsAsFactors = FALSE
)
test_that("count_distinct gives correct results for key types", {
  expect_equal(
    sapply(df_var, n_distinct),
    sapply(df_var, function(.) length(unique(.)))
  )
})

test_that("n_distinct treats NA correctly in the REALSXP case (#384)", {
  expect_equal(n_distinct(c(1.0, NA, NA)), 2)
})
