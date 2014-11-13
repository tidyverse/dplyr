context("Group indices")

test_that("group_indices from ungrouped or grouped gives same result", {
  res1 <- group_indices( mtcars, cyl, vs, am )
  res2 <- mtcars %>% group_by(cyl, vs, am) %>% group_indices()
  expect_equal(res1, res2)  
})

