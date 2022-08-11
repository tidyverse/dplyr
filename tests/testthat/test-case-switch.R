test_that("`NULL` values in `...` are dropped", {
  expect_identical(
    case_switch(1:2, 1 ~ "a", NULL, 2 ~ "b", NULL),
    c("a", "b")
  )
})

test_that("requires at least one condition", {
  expect_snapshot(error = TRUE, {
    case_switch(1)
  })
  expect_snapshot(error = TRUE, {
    case_switch(1, NULL)
  })
})

test_that("passes through `.default` correctly", {
  expect_identical(case_switch(1, 3 ~ 1, .default = 2), 2)
  expect_identical(case_switch(1:5, 6 ~ 1, .default = 2), rep(2, 5))
  expect_identical(case_switch(1:5, 6 ~ 1:5, .default = 2:6), 2:6)
})

test_that("`.default` is part of common type computation", {
  expect_identical(case_switch(1, 1 ~ 1L, .default = 2), 1)

  expect_snapshot(error = TRUE, {
    case_switch(1, 1 ~ 1L, .default = "x")
  })
})

test_that("passes through `.ptype` correctly", {
  expect_identical(case_switch(1, 1 ~ 1, .ptype = integer()), 1L)
})
