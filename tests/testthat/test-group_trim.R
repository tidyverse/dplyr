context("test-group_trim")

test_that("group_trim() is identity on non grouped data", {
  expect_identical(group_trim(iris), iris)
})

test_that("group_trim() always regroups even if no factors", {
  res <- mtcars %>%
    group_by(cyl) %>%
    filter(cyl == 6, .preserve = TRUE) %>%
    group_trim()
  expect_equal(n_groups(res), 1L)
})

test_that("group_trim() drops factor levels in data and grouping structure", {
  res <- iris %>%
    group_by(Species) %>%
    filter(Species == "setosa") %>%
    group_trim()

  expect_equal(n_groups(res), 1L)
  expect_equal(levels(res$Species), "setosa")
  expect_equal(levels(attr(res, "groups")$Species), "setosa")
})
