context("Count")

test_that("can count variable called n", {
  df <- data.frame(n = c(1, 1, 2, 2, 2))

  out <- df %>% count(n)
  expect_equal(names(out), c("n", "nn"))
  expect_equal(out$nn, c(2, 3))

  out <- df %>% count(n, sort = TRUE)
  expect_equal(out$nn, c(3, 2))
})

test_that("grouped count includes group", {
  df <- data.frame(g = c(1, 2, 2, 2))

  res <- df %>% group_by(g) %>% count()
  expect_equal(names(res), c("g", "n"))
  expect_equal(res$n, c(1, 3))
})

# n_distinct --------------------------------------------------------------

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
  expect_equal( n_distinct( c(1.0,NA,NA) ), 2 )
})

