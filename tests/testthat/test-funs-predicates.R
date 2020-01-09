context("funs-predicates")

test_that("all_exprs() creates intersection", {
  expect_identical(all_exprs(am == 1), quo(am == 1))

  quo <- set_env(quo((!!quo(cyl == 2)) & (!!quo(am == 1))), base_env())
  expect_identical(all_exprs(cyl == 2, am == 1), quo)
})

test_that("any_exprs() creates union", {
  expect_identical(any_exprs(am == 1), quo(am == 1))

  quo <- set_env(quo((!!quo(cyl == 2)) | (!!quo(am == 1))), base_env())
  expect_identical(any_exprs(cyl == 2, am == 1), quo)
})

test_that("all_exprs() gives meaningful error messages", {
  verify_output(test_path("test-funs-predicates-errors.txt"), {
    all_exprs()
    any_exprs()
  })
})
