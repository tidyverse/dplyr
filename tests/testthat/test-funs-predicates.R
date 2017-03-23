context("funs-predicates")

test_that("all_of() creates intersection", {
  expect_identical(all_of(am == 1), quo(am == 1))

  quo <- set_env(quo((!! ~cyl == 2) & (!! ~am == 1)), base_env())
  expect_identical(all_of(cyl == 2, am == 1), quo)
})

test_that("any_of() creates union", {
  expect_identical(any_of(am == 1), quo(am == 1))

  quo <- set_env(quo((!! ~cyl == 2) | (!! ~am == 1)), base_env())
  expect_identical(any_of(cyl == 2, am == 1), quo)
})

test_that("all_of() without expression returns an error", {
  expect_error(all_of(), "at least one expression")
})
