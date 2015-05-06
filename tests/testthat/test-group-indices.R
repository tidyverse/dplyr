context("Group indices")

test_that("group_indices from ungrouped or grouped gives same result", {
  res1 <- group_indices( mtcars, cyl, vs, am )
  res2 <- mtcars %>% group_by(cyl, vs, am) %>% group_indices()
  expect_equal(res1, res2)  
})

test_that("group_indices handles the case where no variable is given (#867)", {
  res <- group_indices(mtcars)
  expect_true( all(res==1L) )
})

test_that("group_indices handles grouped data and no arguments", {
  res1 <- mtcars %>% group_by(cyl) %>% group_indices()
  res2 <- mtcars %>% group_indices(cyl)
  expect_equal(res1, res2) 
})

