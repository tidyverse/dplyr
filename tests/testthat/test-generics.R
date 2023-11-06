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

test_that("doesn't expand row names", {
  df <- data.frame(x = 1:10)
  out <- dplyr_col_modify(df, list(y = 1))

  expect_equal(.row_names_info(out, 1), -10)
})

test_that("preserves existing row names", {
  df <- data.frame(x = c(1, 2), row.names = c("a", "b"))
  out <- dplyr_col_modify(df, list(y = 1))
  expect_equal(row.names(df), c("a", "b"))
})

test_that("reconstruct method gets a data frame", {
  seen_df <- NULL

  local_methods(
    dplyr_reconstruct.dplyr_foobar = function(data, template) {
      if (is.data.frame(data)) {
        seen_df <<- TRUE
      }
      NextMethod()
    }
  )

  df <- foobar(data.frame(x = 1))

  seen_df <- FALSE
  dplyr_col_modify(df, list(y = 2))
  expect_true(seen_df)

  seen_df <- FALSE
  dplyr_row_slice(df, 1)
  expect_true(seen_df)
})

# dplyr_reconstruct -------------------------------------------------------

test_that("classes are restored", {
  expect_identical(
    dplyr_reconstruct(tibble(), data.frame()),
    data.frame()
  )
  expect_identical(
    dplyr_reconstruct(tibble(), tibble()),
    tibble()
  )
  expect_identical(
    dplyr_reconstruct(tibble(), new_data_frame(class = "foo")),
    new_data_frame(class = "foo")
  )
})

test_that("attributes of `template` are kept", {
  expect_identical(
    dplyr_reconstruct(new_tibble(list(), nrow = 1), new_data_frame(foo = 1)),
    new_data_frame(n = 1L, foo = 1)
  )
})

test_that("compact row names are retained", {
  data <- vec_rbind(tibble(a = 1), tibble(a = 2))
  template <- tibble()

  x <- dplyr_reconstruct(data, template)
  expect <- tibble(a = c(1, 2))

  expect_identical(x, expect)

  # Explicitly ensure internal row name structure is identical
  expect_identical(
    .row_names_info(x, type = 0L),
    .row_names_info(expect, type = 0L)
  )
})

test_that("dplyr_reconstruct() strips attributes before dispatch", {
  local_methods(
    dplyr_reconstruct.dplyr_foobar = function(data, template) {
      out <<- data
    }
  )

  df <- foobar(data.frame(x = 1), foo = "bar")
  out <- NULL
  dplyr_reconstruct(df, df)
  expect_identical(out, data.frame(x = 1))

  df <- foobar(data.frame(x = 1, row.names = "a"), foo = "bar")
  out <- NULL
  dplyr_reconstruct(df, df)
  expect_identical(out, data.frame(x = 1, row.names = "a"))
})

test_that("`dplyr_reconstruct()` retains attribute ordering of `template`", {
  df <- vctrs::data_frame(x = 1)
  expect_identical(
    attributes(dplyr_reconstruct(df, df)),
    attributes(df)
  )
})

test_that("`dplyr_reconstruct()` doesn't modify the original `data` in place", {
  data <- new_data_frame(list(x = 1), foo = "bar")
  template <- vctrs::data_frame(x = 1)

  out <- dplyr_reconstruct(data, template)

  expect_null(attr(out, "foo"))
  expect_identical(attr(data, "foo"), "bar")
})

test_that("`dplyr_reconstruct()`, which gets and sets attributes, doesn't touch `row.names` (#6525)", {
  skip_if_no_lazy_character()

  dplyr_attributes <- function(x) {
    .Call(ffi_test_dplyr_attributes, x)
  }
  dplyr_set_attributes <- function(x, attributes) {
    .Call(ffi_test_dplyr_set_attributes, x, attributes)
  }

  df <- vctrs::data_frame(x = 1)

  attributes <- attributes(df)
  attributes$row.names <- new_lazy_character(function() "a")
  attributes <- as.pairlist(attributes)

  df_with_lazy_row_names <- dplyr_set_attributes(df, attributes)

  # Ensure `data` row names aren't materialized
  x <- dplyr_reconstruct(df_with_lazy_row_names, df)
  attributes <- dplyr_attributes(df_with_lazy_row_names)
  expect_false(lazy_character_is_materialized(attributes$row.names))

  # `data` row names should also propagate into the result unmaterialized
  attributes <- dplyr_attributes(x)
  expect_false(lazy_character_is_materialized(attributes$row.names))

  # Ensure `template` row names aren't materialized
  x <- dplyr_reconstruct(df, df_with_lazy_row_names)
  attributes <- dplyr_attributes(df_with_lazy_row_names)
  expect_false(lazy_character_is_materialized(attributes$row.names))
})
