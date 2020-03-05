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


# dplyr_col_modify --------------------------------------------------------

test_that("empty cols returns input", {
  df <- data.frame(x = 1)
  expect_equal(dplyr_col_modify(df, list()), df)
})

test_that("applies tidyverse recycling rules", {
  expect_equal(
    dplyr_col_modify(data.frame(x = 1:2), list(y = 1)),
    data.frame(x = 1:2, y = c(1, 1))
  )
  expect_equal(
    dplyr_col_modify(data.frame(x = integer()), list(y = 1)),
    data.frame(x = integer(), y = integer())
  )

  expect_error(
    dplyr_col_modify(data.frame(x = 1:4), list(y = 1:2)),
    class = "vctrs_error_recycle_incompatible_size"
  )
})

test_that("can add, remove, and replace columns", {
  df <- data.frame(x = 1, y = 2)
  expect_equal(dplyr_col_modify(df, list(y = NULL)), data.frame(x = 1))
  expect_equal(dplyr_col_modify(df, list(y = 3)), data.frame(x = 1, y = 3))
  expect_equal(dplyr_col_modify(df, list(z = 3)), data.frame(x = 1, y = 2, z = 3))
})
