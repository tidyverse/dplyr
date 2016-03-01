context("combine")

test_that("combine handles NULL (1596)", {
    expect_equal( combine(list(NULL, 1,2)), c(1,2) )
    expect_equal( combine(list(1,NULL,2)), c(1,2) )
    expect_equal( combine(list(1,2,NULL)), c(1,2) )
    expect_error( combine(list(NULL,NULL)))
})
