context("combine")

test_that("combine handles NULL (1596)", {
    expect_equal( combine(list(NULL, 1,2)), c(1,2) )
    expect_equal( combine(list(1,NULL,2)), c(1,2) )
    expect_equal( combine(list(1,2,NULL)), c(1,2) )
    expect_error( combine(list(NULL,NULL)))
})

test_that("combine complains about incompatibilites", {
  expect_error(combine("a", 1), "character to numeric")
  expect_error(combine(factor("a"), 1L), "factor to integer")
})

test_that("combine works with input that used to fail (#1780)", {
  no <- list(alpha = letters[1:3], omega = letters[24:26])
  expect_equal(combine(no), unlist(no, use.names = FALSE))
})
