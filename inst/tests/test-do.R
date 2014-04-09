context("Do")

grpd <- data.frame(
  g = c(1, 2, 2, 3, 3, 3),
  x = 1:6,
  y = 6:1
) %.% group_by(g)

test_that("can't use both named and unnamed args", {
  expect_error(grpd %.% do(x = 1, 2), "must either be all named or all unnamed")
})

test_that("unnamed elements must return data frames", {
  expect_error(grpd %.% do(1), "not data frames")
  expect_error(grpd %.% do("a"), "not data frames")
})

test_that("unnamed results bounded together by row", {
  first <- grpd %.% do(head(., 1))

  expect_equal(nrow(first), 3)
  expect_equal(first$g, 1:3)
  expect_equal(first$x, c(1, 2, 4))
})

test_that("can only use single unnamed argument", {
  expect_error(grpd %.% do(head, tail), "single unnamed argument")
})

test_that("named argument become list columns", {
  out <- grpd %.% do(nrow = nrow(.), ncol = ncol(.))
  expect_equal(out$nrow, list(1, 2, 3))
  expect_equal(out$ncol, list(3, 3, 3))
})
