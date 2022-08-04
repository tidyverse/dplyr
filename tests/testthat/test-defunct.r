test_that("generate informative errors", {
  expect_snapshot(error = TRUE, {
    id()
    failwith()
    funs()
  })
})
