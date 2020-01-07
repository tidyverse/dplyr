test_that("can selectively ungroup", {
  gf <- group_by(tibble(x = 1, y = 2), x, y)

  expect_equal(gf %>% ungroup() %>% group_vars(), character())
  expect_equal(gf %>% ungroup(everything()) %>% group_vars(), character())
  expect_equal(gf %>% ungroup(x) %>% group_vars(), "y")
  expect_error(gf %>% ungroup(z) %>% group_vars(), "z")
})


test_that("[ method can remove grouping vars", {
  df <- tibble(x = 1, y = 2, z = 3)
  gf <- group_by(df, x, y)

  expect_equal(gf, gf)
  expect_equal(gf[1], group_by(df[1], x))
  expect_equal(gf[3], df[3])
})
