test_that("computes group data when `by` is set", {
  df <- tibble(x = c(1, 1, 2, 2, 1))

  out <- compute_by(by = x, data = df)
  expect_identical(out$type, "grouped")
  expect_identical(out$names, "x")
  expect_identical(out$data, group_data(group_by(df, x)))

  # In particular, sets `drop = TRUE`
  expect_true(attr(out$data, ".drop"))
})

test_that("extracts existing data when `by = NULL`", {
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
