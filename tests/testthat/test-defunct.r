test_that("generate informative errors", {
  expect_snapshot(error = TRUE, {
    id()
    failwith()
    funs()
    select_vars()
    rename_vars()
    select_var()
    current_vars()
  })
})
