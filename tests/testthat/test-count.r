context("Count")

test_that("count_distinct gives the correct results", {
  expect_equal( 
    sapply( iris, count_distinct ), 
    sapply( iris, function(.) length(unique(.)) )
  )
})

