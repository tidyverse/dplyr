test_that("can selectively ungroup", {
  gf <- group_by(tibble(x = 1, y = 2), x, y)

  expect_equal(gf %>% ungroup() %>% group_vars(), character())
  expect_equal(gf %>% ungroup(everything()) %>% group_vars(), character())
  expect_equal(gf %>% ungroup(x) %>% group_vars(), "y")
  expect_error(gf %>% ungroup(z) %>% group_vars(), "z")
})
