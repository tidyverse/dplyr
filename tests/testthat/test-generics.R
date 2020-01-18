test_that("row_slice recomputes groups", {
  gf <- group_by(data.frame(g = c(1, 1, 2, 2, 3, 3)), g)
  out <- dplyr_row_slice(gf, c(1L, 3L, 5L))
  expect_equal(group_data(out)$.rows, list_of(1L, 2L, 3L))

  out <- dplyr_row_slice(gf, c(4L, 3L))
  expect_equal(group_data(out)$.rows, list_of(c(1L, 2L)))
})

test_that("row_slice preserves empty groups if requested", {
  gf <- group_by(data.frame(g = c(1, 1, 2, 2, 3, 3)), g, .drop = FALSE)
  out <- dplyr_row_slice(gf, c(3L, 4L))
  expect_equal(group_data(out)$.rows, list_of(integer(), c(1L, 2L), integer()))
})
