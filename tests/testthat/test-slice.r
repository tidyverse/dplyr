context("slice")

test_that( "slice handles numeric input (#226)", {
  g <- mtcars %>% group_by(cyl)
  res <- g %>% slice(1)
  expect_equal(nrow(res), 3)
  expect_equal(res, g %>% filter(row_number()==1L))
  
  expect_equal(mtcars %>% slice(1), mtcars %>% filter(row_number() == 1L) )
})

test_that( "slice silently ignores out of range values (#226)", {
  expect_equal( slice(mtcars, c(-3,2,100)), slice(mtcars, 2))
  
  g <- group_by(mtcars, cyl)
  expect_equal( slice(g, c(-3,2,100)), slice(g, 2))
  
})

