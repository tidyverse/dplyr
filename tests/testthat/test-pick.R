test_that("can pick columns from the data", {
  df <- tibble(x1 = 1, y = 2, x2 = 3, z = 4)
  out <- mutate(df, sel = pick(z, starts_with("x")))
  expect_identical(out$sel, df[c("z", "x1", "x2")])
})

test_that("returns separate data frames for each group", {
  fn <- function(x) {
    x[["x"]] + mean(x[["z"]])
  }

  df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5, z = 11:15)
  gdf <- group_by(df, g)

  out <- mutate(gdf, res = fn(pick(x, z)))
  expect <- mutate(gdf, res = x + mean(z))

  expect_identical(out, expect)
})

test_that("returns a tibble", {
  df <- data.frame(x = 1)
  out <- mutate(df, y = pick(x))
  expect_s3_class(out$y, "tbl_df")
})

test_that("returns a list-col with `rowwise()` data (#5951, #6264)", {
  # This replaces the `across(.fns = NULL)` behavior on rowwise-dfs
  df <- tibble(x = list(1, 2:3, 4:5), y = 1:3)
  rdf <- rowwise(df)

  out <- mutate(rdf, z = pick(x, y))
  expect_identical(out$z, df)

  out <- mutate(rdf, z = is_list(pick(x)$x))
  expect_identical(out$z, c(TRUE, TRUE, TRUE))
})

test_that("selectors won't select grouping columns", {
  df <- tibble(g = 1, x = 2)
  gdf <- group_by(df, g)

  out <- mutate(gdf, y = pick(everything()))
  expect_named(out$y, "x")
})

test_that("can't explicitly select grouping columns (#5460)", {
  # Related to removing the mask layer from the quosure environments
  df <- tibble(g = 1, x = 2)
  gdf <- group_by(df, g)

  expect_snapshot(error = TRUE, {
    mutate(gdf, y = pick(g))
  })
})

test_that("`all_of()` is evaluated in the correct environment (#5460)", {
  # Related to removing the mask layer from the quosure environments
  df <- tibble(g = 1, x = 2, y = 3)

  expect_snapshot(error = TRUE, {
    mutate(df, z = pick(all_of(y)))
  })

  y <- "x"
  out <- mutate(df, z = pick(all_of(y)))
  expect_identical(out$z, df["x"])
})

test_that("works with empty selections", {
  df <- tibble(g = c(1, 1, 2), x = c(2, 3, 4))
  gdf <- group_by(df, g)

  out <- mutate(gdf, y = pick(starts_with("foo")))
  expect_identical(out$y, new_tibble(x = list(), nrow = 3))
})

test_that("`...` are evaluated on the full column, not the current group chop", {
  # To try and ensure tidyselection is consistent across groups, in case
  # we can ever optimize to do 1 selection up front and use it on all groups
  df <- tibble(g = c(1, 1, 2, 2), x = c(0, 0, 1, 1))
  gdf <- group_by(df, g)

  out <- mutate(gdf, y = pick(where(~all(.x == 0))))

  # Don't expect any columns, because `pick(...)` should be
  # evaluated on full `x` column
  expect_named(out$y, character())
})

test_that("`pick()` can be used inside `group_by()` wrappers", {
  tidyselect_group_by <- function(data, groups) {
    group_by(data, pick({{ groups }}))
  }

  df <- tibble(a = 1:3, b = 2:4, c= 3:5)

  expect_identical(
    tidyselect_group_by(df, c(a, c)),
    group_by(df, a, c)
  )
})

test_that("uses the current view of the data when called sequentially", {
  df <- tibble(x = 1)
  expect <- tibble(x = tibble(x = tibble(x = 1)))
  expect_identical(mutate(df, x = pick(x), x = pick(x)), expect)
})

test_that("can call different `pick()` expressions in different groups", {
  df <- tibble(g = c(1, 2), x = 1:2, y = 3:4)
  gdf <- group_by(df, g)

  out <- mutate(gdf, z = if (g == 1) pick(x) else pick(y))
  expect_identical(out$z, tibble(x = c(1L, NA), y = c(NA, 4L)))
})

test_that("can call `pick()` from a user defined function", {
  df <- tibble(a = 1, b = 2, c = 3)

  my_pick <- function() pick(a, c)

  out <- mutate(df, d = my_pick())

  expect_identical(out$d, df[c("a", "c")])
})

test_that("errors correctly outside mutate context", {
  expect_snapshot(error = TRUE, {
    pick()
  })
  expect_snapshot(error = TRUE, {
    pick(a, b)
  })
})

test_that("requires at least one input", {
  expect_snapshot(error = TRUE, {
    mutate(data.frame(), pick())
  })
})

test_that("doesn't allow renaming", {
  expect_snapshot(error = TRUE, {
    mutate(data.frame(x = 1), pick(y = x))
  })
})
