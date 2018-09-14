context("n_distinct")

test_that("n_distinct gives the correct results on iris", {
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
test_that("n_distinct gives correct results for key types", {
  expect_equal(
    sapply(df_var, n_distinct),
    sapply(df_var, function(.) length(unique(.)))
  )
})

test_that("n_distinct treats NA correctly in the REALSXP case (#384)", {
  expect_equal(n_distinct(c(1.0, NA, NA)), 2)
})

test_that("n_distinct recycles length 1 vectors (#3685)", {
  expect_equal(n_distinct(1, 1:4), 4)
  expect_equal(n_distinct(1:4, 1), 4)
  expect_error(n_distinct(1:2, 1:3))

  d <- tibble(x = 1:4)
  res <- d %>%
    summarise(y = sum(x), n1 = n_distinct(y, x), n2 = n_distinct(x, y), n3 = n_distinct(y), n4 = n_distinct(identity(y)), n5 = n_distinct(x))
  expect_equal(res$n1, 4)
  expect_equal(res$n2, 4)
  expect_equal(res$n3, 1)
  expect_equal(res$n4, 1)
  expect_equal(res$n5, 4)

  res <- tibble(g = c(1,1,1,1,2,2), x = c(1,2,3,1,1,2)) %>%
    group_by(g) %>%
    summarise(y = sum(x), n1 = n_distinct(y, x), n2 = n_distinct(x, y), n3 = n_distinct(y), n4 = n_distinct(identity(y)), n5 = n_distinct(x))
  expect_equal(res$n1, c(3,2))
  expect_equal(res$n2, c(3,2))
  expect_equal(res$n3, c(1,1))
  expect_equal(res$n4, c(1,1))
  expect_equal(res$n5, c(3,2))
})

test_that("n_distinct handles expressions in na.rm (#3686)", {
  d <- tibble(x = c(1:4,NA))
  yes <- TRUE
  no <- FALSE
  expect_equal(d %>% summarise(n = n_distinct(x, na.rm = T)) %>% pull(), 4)
  expect_equal(d %>% summarise(n = n_distinct(x, na.rm = F)) %>% pull(), 5)

  expect_equal(d %>% summarise(n = n_distinct(x, na.rm = yes)) %>% pull(), 4)
  expect_equal(d %>% summarise(n = n_distinct(x, na.rm = no)) %>% pull(), 5)

  expect_equal(d %>% summarise(n = n_distinct(x, na.rm = TRUE || TRUE)) %>% pull(), 4)
})
