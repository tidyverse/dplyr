test_that("`morph()` allows summaries", {
  df <- tibble(g = c(1, 1, 1, 2, 2), x = 1:5)

  expect_identical(
    morph(df, x = mean(x)),
    tibble(x = 3)
  )
  expect_identical(
    morph(df, x = mean(x), .by = g),
    tibble(g = c(1, 2), x = c(2, 4.5))
  )
})

test_that("`morph()` allows size 0 results", {
  df <- tibble(g = c(1, 1, 1, 2, 2), x = 1:5)
  gdf <- group_by(df, g)

  expect_identical(
    morph(df, x = which(x > 5)),
    tibble(x = integer())
  )
  expect_identical(
    morph(df, x = which(x > 5), .by = g),
    tibble(g = double(), x = integer())
  )
  expect_identical(
    morph(gdf, x = which(x > 5)),
    tibble(g = double(), x = integer())
  )
})

test_that("`morph()` allows size >1 results", {
  df <- tibble(g = c(1, 1, 1, 2, 2), x = 1:5)
  gdf <- group_by(df, g)

  expect_identical(
    morph(df, x = which(x > 2)),
    tibble(x = 3:5)
  )
  expect_identical(
    morph(df, x = which(x > 2), .by = g),
    tibble(g = c(1, 2, 2), x = c(3L, 1L, 2L))
  )
  expect_identical(
    morph(gdf, x = which(x > 2)),
    tibble(g = c(1, 2, 2), x = c(3L, 1L, 2L))
  )
})

test_that("`morph()` recycles across columns", {
  df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)

  out <- morph(df, a = 1:2, b = 1L, c = 2:3)
  expect_identical(out$a, 1:2)
  expect_identical(out$b, c(1L, 1L))
  expect_identical(out$c, 2:3)

  out <- morph(df, a = 1:2, b = 1L, c = 2:3, .by = g)
  expect_identical(out$g, c(1, 1, 2, 2))
  expect_identical(out$a, c(1:2, 1:2))
  expect_identical(out$b, c(1L, 1L, 1L, 1L))
  expect_identical(out$c, c(2:3, 2:3))
})

test_that("`morph()` can recycle across columns to size 0", {
  df <- tibble(g = 1:2, x = 1:2)
  gdf <- group_by(df, g)

  out <- morph(df, y = mean(x), z = which(x > 3))
  expect_identical(out$y, double())
  expect_identical(out$z, integer())

  out <- morph(df, y = mean(x), z = which(x > 1), .by = g)
  expect_identical(out$g, 2L)
  expect_identical(out$y, 2)
  expect_identical(out$z, 1L)

  out <- morph(gdf, y = mean(x), z = which(x > 1))
  expect_identical(out$g, 2L)
  expect_identical(out$y, 2)
  expect_identical(out$z, 1L)
})

test_that("`morph()` throws intelligent recycling errors", {
  df <- tibble(g = 1:2, x = 1:2)
  gdf <- group_by(df, g)

  expect_snapshot(error = TRUE, {
    morph(df, x = 1:2, y = 3:5)
  })
  expect_snapshot(error = TRUE, {
    morph(df, x = 1:2, y = 3:5, .by = g)
  })
  expect_snapshot(error = TRUE, {
    morph(gdf, x = 1:2, y = 3:5)
  })
})

test_that("`morph()` can return more rows than the original data frame", {
  df <- tibble(x = 1:2)

  expect_identical(
    morph(df, x = vec_rep_each(x, x)),
    tibble(x = c(1L, 2L, 2L))
  )
})

test_that("`morph()` doesn't message about regrouping when multiple group columns are supplied", {
  df <- tibble(a = c(1, 1, 2, 2, 2), b = c(1, 2, 1, 1, 2), x = 1:5)
  gdf <- group_by(df, a, b)

  # Silence
  expect_snapshot({
    out <- morph(df, x = mean(x), .by = c(a, b))
  })
  expect_snapshot({
    out <- morph(gdf, x = mean(x))
  })
})

test_that("`morph()` doesn't message about regrouping when >1 rows are returned per group", {
  df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)
  gdf <- group_by(df, g)

  # Silence
  expect_snapshot({
    out <- morph(df, x = vec_rep_each(x, x), .by = g)
  })
  expect_snapshot({
    out <- morph(gdf, x = vec_rep_each(x, x))
  })
})

test_that("`morph()` allows sequential assignments", {
  df <- tibble(g = 1:2, x = 1:2)

  expect_identical(
    morph(df, y = 3, z = mean(x) + y),
    tibble(y = 3, z = 4.5)
  )
  expect_identical(
    morph(df, y = 3, z = mean(x) + y, .by = g),
    tibble(g = 1:2, y = c(3, 3), z = c(4, 5))
  )
})

test_that("`morph()` allows for overwriting existing columns", {
  df <- tibble(g = c("a", "b"), x = 1:2)

  expect_identical(
    morph(df, x = 3, z = x),
    tibble(x = 3, z = 3)
  )
  expect_identical(
    morph(df, x = cur_group_id(), z = x, .by = g),
    tibble(g = c("a", "b"), x = 1:2, z = 1:2)
  )
})

test_that("`morph()` works with unquoted values", {
  df <- tibble(x = 1:5)
  expect_equal(morph(df, out = !!1), tibble(out = 1))
  expect_equal(morph(df, out = !!quo(1)), tibble(out = 1))
  expect_equal(morph(df, out = !!(1:2)), tibble(out = 1:2))
})

test_that("`morph()` with bare data frames always returns a bare data frame", {
  df <- data.frame(g = c(1, 1, 2, 1, 2), x = c(5, 2, 1, 2, 3))

  out <- morph(df, x = mean(x))
  expect_s3_class(out, class(df), exact = TRUE)

  out <- morph(df, x = mean(x), .by = g)
  expect_s3_class(out, class(df), exact = TRUE)
})

test_that("`morph()` drops data frame attributes", {
  # Because `morph()` theoretically creates a "new" data frame

  # With data.frames
  df <- data.frame(g = c(1, 1, 2), x = c(1, 2, 1))
  attr(df, "foo") <- "bar"

  out <- morph(df, x = mean(x))
  expect_null(attr(out, "foo"))

  out <- morph(df, x = mean(x), .by = g)
  expect_null(attr(out, "foo"))

  # With tibbles
  tbl <- as_tibble(df)
  attr(tbl, "foo") <- "bar"

  out <- morph(tbl, x = mean(x))
  expect_null(attr(out, "foo"))

  out <- morph(tbl, x = mean(x), .by = g)
  expect_null(attr(out, "foo"))

  # With grouped_df
  gdf <- group_by(df, g)
  attr(gdf, "foo") <- "bar"

  out <- morph(gdf, x = mean(x))
  expect_null(attr(out, "foo"))
})

test_that("`morph()` with `group_by()` sorts keys", {
  df <- tibble(g = c(2, 1, 2, 0), x = c(4, 2, 8, 5))
  df <- group_by(df, g)

  out <- morph(df, x = mean(x))

  expect_identical(out$g, c(0, 1, 2))
  expect_identical(out$x, c(5, 2, 6))
})

test_that("`morph()` with `group_by()` respects `.drop = FALSE`", {
  g <- factor(c("c", "a", "c"), levels = c("a", "b", "c"))

  df <- tibble(g = g, x = c(1, 4, 2))
  gdf <- group_by(df, g, .drop = FALSE)

  out <- morph(gdf, x = mean(x))

  expect_identical(out$g, factor(c("a", "b", "c")))
  expect_identical(out$x, c(4, NaN, 1.5))
})

test_that("`morph()` with `group_by()` always returns an ungrouped tibble", {
  df <- tibble(a = c(1, 1, 2, 2, 2), b = c(1, 2, 1, 1, 2), x = 1:5)
  gdf <- group_by(df, a, b)

  out <- morph(gdf, x = mean(x))
  expect_identical(class(out), class(df))
})

test_that("`morph()` with `rowwise()` respects list-col element access", {
  df <- tibble(x = list(1:2, 3:5, 6L))
  rdf <- rowwise(df)

  expect_identical(
    morph(rdf, x),
    tibble(x = 1:6)
  )
})

test_that("`morph()` with `rowwise()` respects rowwise group columns", {
  df <- tibble(g = c(1, 1, 2), x = list(1:2, 3:5, 6L))
  rdf <- rowwise(df, g)

  out <- morph(rdf, x)
  expect_identical(out$g, c(rep(1, 5), 2))
  expect_identical(out$x, 1:6)
})

test_that("`morph()` with `rowwise()` always returns an ungrouped tibble", {
  df <- tibble(g = c(1, 1, 2), x = list(1:2, 3:5, 6L))
  rdf <- rowwise(df, g)

  expect_s3_class(morph(rdf, x), class(df), exact = TRUE)
})

# .by ----------------------------------------------------------------------

test_that("can group transiently using `.by`", {
  df <- tibble(g = c(1, 1, 2, 1, 2), x = c(5, 2, 1, 2, 3))

  out <- morph(df, x = mean(x), .by = g)

  expect_identical(out$g, c(1, 2))
  expect_identical(out$x, c(3, 2))
  expect_s3_class(out, class(df), exact = TRUE)
})

test_that("transient grouping orders by first appearance", {
  df <- tibble(g = c(2, 1, 2, 0), x = c(4, 2, 8, 5))

  out <- morph(df, x = mean(x), .by = g)

  expect_identical(out$g, c(2, 1, 0))
  expect_identical(out$x, c(6, 2, 5))
})

test_that("catches `.by` with grouped-df", {
  df <- tibble(x = 1)
  gdf <- group_by(df, x)

  expect_snapshot(error = TRUE, {
    morph(gdf, .by = x)
  })
})

test_that("catches `.by` with rowwise-df", {
  df <- tibble(x = 1)
  rdf <- rowwise(df)

  expect_snapshot(error = TRUE, {
    morph(rdf, .by = x)
  })
})
