test_that("computes group data when `by` is set", {
  df <- tibble(x = c(1, 1, 2, 2, 1))

  out <- compute_by(by = x, data = df)
  expect_identical(out$type, "grouped")
  expect_identical(out$names, "x")

  expect_identical(
    out$data,
    tibble(x = c(1, 2), ".rows" := list_of(c(1L, 2L, 5L), c(3L, 4L)))
  )
})

test_that("computes `by` group data in order of appearance", {
  df <- tibble(
    x = c(5, 4, 5, 5),
    y = c(2, 3, 1, 2)
  )

  out <- compute_by(by = c(x, y), data = df)

  expect <- tibble(
    x = c(5, 4, 5),
    y = c(2, 3, 1),
    ".rows" := list_of(c(1L, 4L), 2L, 3L)
  )

  expect_identical(out$data, expect)
})

test_that("extracts existing data when `by = NULL`", {
  df <- data.frame(x = c(1, 1, 2, 2, 1))
  out <- compute_by(by = NULL, data = df)
  expect_identical(out$type, "ungrouped")
  expect_identical(out$names, character())
  # `compute_by()` is always type stable on `$data` and returns a bare tibble
  expect_identical(out$data, as_tibble(group_data(df)))

  df <- tibble(x = c(1, 1, 2, 2, 1))
  out <- compute_by(by = NULL, data = df)
  expect_identical(out$type, "ungrouped")
  expect_identical(out$names, character())
  expect_identical(out$data, group_data(df))

  gdf <- group_by(df, x)
  out <- compute_by(by = NULL, data = gdf)
  expect_identical(out$type, "grouped")
  expect_identical(out$names, "x")
  expect_identical(out$data, group_data(gdf))

  rdf <- rowwise(df)
  out <- compute_by(by = NULL, data = rdf)
  expect_identical(out$type, "rowwise")
  expect_identical(out$names, character())
  expect_identical(out$data, group_data(rdf))
})

test_that("empty selection results in ungrouped group data", {
  df <- tibble(x = 1)

  out <- compute_by(by = c(), data = df)
  expect_identical(out$type, "ungrouped")
  expect_identical(out$names, character())
  expect_identical(out$data, group_data(df))
})

test_that("throws tidyselect errors", {
  df <- tibble(x = 1)

  expect_snapshot(error = TRUE, {
    compute_by(by = y, data = df)
  })
})

test_that("can't set `.by` with a grouped-df", {
  df <- tibble(x = 1:5)
  gdf <- group_by(df, x)

  expect_snapshot(error = TRUE, {
    compute_by(x, gdf)
  })
})

test_that("can't set `.by` with a rowwise-df", {
  df <- tibble(x = 1:5)
  rdf <- rowwise(df)

  expect_snapshot(error = TRUE, {
    compute_by(x, rdf)
  })
})

test_that("can tweak the error args", {
  df <- tibble(x = 1:5)
  gdf <- group_by(df, x)

  expect_snapshot(error = TRUE, {
    compute_by(x, gdf, by_arg = "x", data_arg = "dat")
  })
})
