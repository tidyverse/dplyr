# ------------------------------------------------------------------------------
# pick() + mutate()

test_that("can pick columns from the data", {
  df <- tibble(x1 = 1, y = 2, x2 = 3, z = 4)

  expect <- df[c("z", "x1", "x2")]

  out <- mutate(df, sel = pick(z, starts_with("x")))
  expect_identical(out$sel, expect)

  out <- mutate(df, sel = pick_wrapper(z, starts_with("x")))
  expect_identical(out$sel, expect)
})

test_that("can use namespaced call to `pick()`", {
  df <- tibble(x = 1, y = "y")

  expect_identical(
    mutate(df, z = dplyr::pick(where(is.character))),
    mutate(df, z = pick(where(is.character)))
  )
})

test_that("returns separate data frames for each group", {
  fn <- function(x) {
    x[["x"]] + mean(x[["z"]])
  }

  df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5, z = 11:15)
  gdf <- group_by(df, g)

  expect <- mutate(gdf, res = x + mean(z))

  out <- mutate(gdf, res = fn(pick(x, z)))
  expect_identical(out, expect)

  out <- mutate(gdf, res = fn(pick_wrapper(x, z)))
  expect_identical(out, expect)
})

test_that("returns a tibble", {
  df <- data.frame(x = 1)

  out <- mutate(df, y = pick(x))
  expect_s3_class(out$y, "tbl_df")

  out <- mutate(df, y = pick_wrapper(x))
  expect_s3_class(out$y, "tbl_df")
})

test_that("with `rowwise()` data, leaves list-cols unwrapped (#5951, #6264)", {
  # Because this most closely mimics macro expansion of:
  # pick(x) -> tibble(x = x)
  df <- tibble(x = list(1, 2:3, 4:5), y = 1:3)
  rdf <- rowwise(df)

  expect_snapshot(error = TRUE, {
    mutate(rdf, z = pick(x, y))
  })
  expect_snapshot(error = TRUE, {
    mutate(rdf, z = pick_wrapper(x, y))
  })
})

test_that("selectors won't select grouping columns", {
  df <- tibble(g = 1, x = 2)
  gdf <- group_by(df, g)

  out <- mutate(gdf, y = pick(everything()))
  expect_named(out$y, "x")

  out <- mutate(gdf, y = pick_wrapper(everything()))
  expect_named(out$y, "x")
})

test_that("selectors won't select rowwise 'grouping' columns", {
  df <- tibble(g = 1, x = 2)
  rdf <- rowwise(df, g)

  out <- mutate(rdf, y = pick(everything()))
  expect_named(out$y, "x")

  out <- mutate(rdf, y = pick_wrapper(everything()))
  expect_named(out$y, "x")
})

test_that("can't explicitly select grouping columns (#5460)", {
  # Related to removing the mask layer from the quosure environments
  df <- tibble(g = 1, x = 2)
  gdf <- group_by(df, g)

  expect_snapshot(error = TRUE, {
    mutate(gdf, y = pick(g))
  })
  expect_snapshot(error = TRUE, {
    mutate(gdf, y = pick_wrapper(g))
  })
})

test_that("`all_of()` is evaluated in the correct environment (#5460)", {
  # Related to removing the mask layer from the quosure environments
  df <- tibble(g = 1, x = 2, y = 3)

  expect_snapshot(error = TRUE, {
    mutate(df, z = pick(all_of(y)))
  })
  expect_snapshot(error = TRUE, {
    mutate(df, z = pick_wrapper(all_of(y)))
  })

  y <- "x"
  expect <- df["x"]

  out <- mutate(df, z = pick(all_of(y)))
  expect_identical(out$z, expect)

  out <- mutate(df, z = pick_wrapper(all_of(y)))
  expect_identical(out$z, expect)
})

test_that("empty selections create 0 row data frames", {
  # Most closely mimics macro expansion of `pick(<empty-sel>) -> tibble()`.
  # Easily explainable as such.
  df <- tibble(g = c(1, 1, 2), x = c(2, 3, 4))
  gdf <- group_by(df, g)

  expect_snapshot(error = TRUE, {
    mutate(gdf, y = pick(starts_with("foo")))
  })
  expect_snapshot(error = TRUE, {
    mutate(gdf, y = pick_wrapper(starts_with("foo")))
  })
})

test_that("must supply at least one selector to `pick()`", {
  df <- tibble(x = c(2, 3, 4))

  expect_snapshot(error = TRUE, {
    mutate(df, y = pick())
  })
  expect_snapshot(error = TRUE, {
    mutate(df, y = pick_wrapper())
  })
})

test_that("the tidyselection and column extraction are evaluated on the current data", {
  # Because `pick()` is viewed as macro expansion, and the expansion inherits
  # typical dplyr semantics

  df <- tibble(g = c(1, 2, 2), x = 1:3)
  gdf <- group_by(df, g)

  expect_snapshot(error = TRUE, {
    # Expands to `tibble(x = x)`
    mutate(gdf, x = NULL, y = pick(x))
  })
  expect_snapshot(error = TRUE, {
    # Does actual `eval_select()` call per group
    mutate(gdf, x = NULL, y = pick_wrapper(x))
  })

  # Can select newly created columns
  out <- mutate(gdf, y = x + 1L, z = pick(x, y))
  expect_identical(out[c("x", "y")], out$z)

  out <- mutate(gdf, y = x + 1L, z = pick_wrapper(x, y))
  expect_identical(out[c("x", "y")], out$z)


  df <- tibble(x = 1)
  expect <- tibble(x = tibble(x = tibble(x = 1)), y = tibble(x = x))

  out <- mutate(df, x = pick(x), x = pick(x), y = pick(x))
  expect_identical(out, expect)

  out <- mutate(df, x = pick_wrapper(x), x = pick_wrapper(x), y = pick_wrapper(x))
  expect_identical(out, expect)
})

test_that("can call different `pick()` expressions in different groups", {
  df <- tibble(g = c(1, 2), x = 1:2, y = 3:4)
  gdf <- group_by(df, g)

  expect <- tibble(x = c(1L, NA), y = c(NA, 4L))

  out <- mutate(gdf, z = if (g == 1) pick(x) else pick(y))
  expect_identical(out$z, expect)

  out <- mutate(gdf, z = if (g == 1) pick_wrapper(x) else pick_wrapper(y))
  expect_identical(out$z, expect)
})

test_that("can call `pick()` from a user defined function", {
  df <- tibble(a = 1, b = 2, c = 3)
  gdf <- group_by(df, a)

  # Hardcoded variables in expression
  my_pick <- function() pick(a, c)
  out <- mutate(df, d = my_pick())
  expect_identical(out$d, df[c("a", "c")])

  # Hardcoded `all_of()` using a local variable
  my_pick <- function() {
    x <- c("a", "c")
    pick(all_of(x))
  }
  out <- mutate(df, d = my_pick())
  expect_identical(out$d, df[c("a", "c")])

  expect_snapshot(error = TRUE, {
    mutate(gdf, d = my_pick())
  })

  # Dynamic `all_of()` using user supplied variable
  my_pick <- function(x) {
    pick(all_of(x))
  }
  y <- c("a", "c")
  out <- mutate(df, d = my_pick(y))
  expect_identical(out$d, df[c("a", "c")])

  expect_snapshot(error = TRUE, {
    mutate(gdf, d = my_pick(y))
  })
})

test_that("wrapped `all_of()` and `where()` selections work", {
  df <- tibble(a = 1, b = "x", c = 3)

  my_pick <- function(x) {
    pick(all_of(x))
  }
  out <- mutate(df, x = my_pick("a"), y = my_pick("b"))
  expect_identical(out$x, df["a"])
  expect_identical(out$y, df["b"])

  my_pick2 <- function(x) {
    pick(all_of(x))
  }
  out <- mutate(df, x = my_pick("a"), y = my_pick2("b"))
  expect_identical(out$x, df["a"])
  expect_identical(out$y, df["b"])

  my_where <- function(fn) {
    pick(where(fn))
  }
  out <- mutate(df, x = my_where(is.numeric), y = my_where(is.character))
  expect_identical(out$x, df[c("a", "c")])
  expect_identical(out$y, df["b"])
})

test_that("`pick()` expansion evaluates on the full data", {
  # To ensure tidyselection is consistent across groups
  df <- tibble(g = c(1, 1, 2, 2), x = c(0, 0, 1, 1), y = c(1, 1, 0, 0))
  gdf <- group_by(df, g)

  # Doesn't select any columns
  expect_snapshot(error = TRUE, {
    mutate(gdf, y = pick(where(~all(.x == 0))))
  })

  # `pick()` evaluation fallback evaluates on the group specific data,
  # forcing potentially different results per group.
  out <- mutate(gdf, z = pick_wrapper(where(~all(.x == 0))))
  expect_named(out$z, c("x", "y"))
  expect_identical(out$z$x, c(0, 0, NA, NA))
  expect_identical(out$z$y, c(NA, NA, 0, 0))
})

test_that("`pick()` expansion/tidyselection happens outside the data mask", {
  # `pick()` expressions are evaluated in the caller environment of the verb.
  # This is intentional to avoid theoretical per-group differences in what
  # `pick()` should return.
  df <- tibble(x = 1, y = 2, z = 3)

  a <- "z"
  expect <- df["z"]

  out <- mutate(df, foo = {
    a <- "x"
    pick(all_of(a))
  })
  expect_identical(out$foo, expect)

  # `pick()`'s evaluation fallback also performs the tidy-selection
  # in the calling environment of the verb
  out <- mutate(df, foo = {
    a <- "x"
    pick_wrapper(all_of(a))
  })
  expect_identical(out$foo, expect)
})

test_that("errors correctly outside mutate context", {
  expect_snapshot(error = TRUE, {
    pick()
  })
  expect_snapshot(error = TRUE, {
    pick(a, b)
  })
})

test_that("can assign `pick()` to new function", {
  # Will run the evaluation version of `pick()`
  pick2 <- pick

  df <- tibble(x = 1, y = 2)

  out <- mutate(df, z = pick2(y))
  expect_identical(out$z, df["y"])
})

test_that("selection on rowwise data frames uses full list-cols, but actual evaluation unwraps them", {
  df <- tibble(x = list(1:2, 2:4, 5))
  df <- rowwise(df)

  # i.e. can select based on list-ness of the column.
  # Expands to `y = list(tibble(x = x))` where `x` is `1:2`, `2:4`, `5` like it
  # would be if you called that directly.
  out <- mutate(df, y = list(pick(where(is.list))))
  expect_identical(out$y, map(df$x, ~tibble(x = .x)))
})

test_that("doesn't allow renaming", {
  expect_snapshot(error = TRUE, {
    mutate(data.frame(x = 1), pick(y = x))
  })
  expect_snapshot(error = TRUE, {
    mutate(data.frame(x = 1), pick_wrapper(y = x))
  })
})

# ------------------------------------------------------------------------------
# pick() + summarise()/reframe()

test_that("can `pick()` inside `reframe()`", {
  df <- tibble(g = c(1, 1, 2, 1, 2), x = c(1, 1, 1, 2, 2), y = c(1, 1, 1, 2, 1))
  gdf <- group_by(df, g)

  expect_key <- df[c(1, 4, 5), c("x", "y")]
  expect_count <- c(3L, 1L, 1L)

  out <- reframe(df, vec_count(pick(x, y), sort = "count"))
  expect_identical(out$key, expect_key)
  expect_identical(out$count, expect_count)

  out <- reframe(df, vec_count(pick_wrapper(x, y), sort = "count"))
  expect_identical(out$key, expect_key)
  expect_identical(out$count, expect_count)


  expect_key <- df[c(1, 4, 3, 5), c("x", "y")]
  expect_count <- c(2L, 1L, 1L, 1L)

  out <- reframe(gdf, vec_count(pick(x, y), sort = "count"))
  expect_identical(out$key, expect_key)
  expect_identical(out$count, expect_count)

  out <- reframe(gdf, vec_count(pick_wrapper(x, y), sort = "count"))
  expect_identical(out$key, expect_key)
  expect_identical(out$count, expect_count)
})

test_that("recycles correctly with empty selection", {
  df <- tibble(x = 1:5)

  out <- reframe(df, sum = sum(x), y = pick(starts_with("foo")))
  expect_identical(out$sum, integer())
  expect_identical(out$y, new_tibble(list(), nrow = 0L))

  out <- reframe(df, sum = sum(x), y = pick_wrapper(starts_with("foo")))
  expect_identical(out$sum, integer())
  expect_identical(out$y, new_tibble(list(), nrow = 0L))
})

test_that("uses 'current' columns of `summarize()` and `reframe()`", {
  df <- tibble(x = 1:5, y = 6:10)

  # Uses size of current version of `x`
  expect_x <- 15L
  expect_z <- tibble(x = 15L)

  out <- summarise(df, x = sum(x), z = pick(x))
  expect_identical(out$x, expect_x)
  expect_identical(out$z, expect_z)

  out <- summarise(df, x = sum(x), z = pick_wrapper(x))
  expect_identical(out$x, expect_x)
  expect_identical(out$z, expect_z)


  # Adding in `y` forces recycling
  expect_x <- vec_rep(15L, 5)
  expect_z <- tibble(x = 15L, y = 6:10)

  out <- reframe(df, x = sum(x), z = pick(x, y))
  expect_identical(out$x, expect_x)
  expect_identical(out$z, expect_z)

  out <- reframe(df, x = sum(x), z = pick_wrapper(x, y))
  expect_identical(out$x, expect_x)
  expect_identical(out$z, expect_z)
})

test_that("can select completely new columns in `summarise()`", {
  df <- tibble(x = 1:5)

  out <- mutate(df, y = x + 1, z = pick(y))
  expect_identical(out["y"], out$z)

  out <- mutate(df, y = x + 1, z = pick_wrapper(y))
  expect_identical(out["y"], out$z)
})

# ------------------------------------------------------------------------------
# pick() + arrange()

test_that("can `arrange()` with `pick()` selection", {
  df <- tibble(x = c(2, 2, 1), y = c(3, 1, 3))

  expect <- df[c(3, 2, 1),]

  expect_identical(arrange(df, pick(x, y)), expect)
  expect_identical(arrange(df, pick_wrapper(x, y)), expect)

  expect_identical(arrange(df, pick(x), y), expect)
  expect_identical(arrange(df, pick_wrapper(x), y), expect)
})

test_that("`pick()` errors in `arrange()` are useful", {
  df <- tibble(x = 1)

  expect_snapshot(error = TRUE, {
    arrange(df, pick(y))
  })
  expect_snapshot(error = TRUE, {
    arrange(df, foo(pick(x)))
  })
})

# ------------------------------------------------------------------------------
# pick() + filter()

test_that("can `pick()` inside `filter()`", {
  df <- tibble(x = c(1, 2, NA, 3), y = c(2, NA, 5, 3))

  out <- filter(df, vec_detect_complete(pick(x, y)))
  expect_identical(out, df[c(1, 4),])

  out <- filter(df, vec_detect_complete(pick_wrapper(x, y)))
  expect_identical(out, df[c(1, 4),])
})

test_that("`filter()` with `pick()` that uses invalid tidy-selection errors", {
  df <- tibble(x = c(1, 2, NA, 3), y = c(2, NA, 5, 3))

  expect_snapshot(error = TRUE, {
    filter(df, pick(x, a))
  })
  expect_snapshot(error = TRUE, {
    filter(df, pick_wrapper(x, a))
  })
})

test_that("`filter()` that doesn't use `pick()` result correctly errors", {
  df <- tibble(x = c(1, 2, NA, 3), y = c(2, NA, 5, 3))

  # TODO: Can we improve on the `In argument:` expression in the expansion case?
  expect_snapshot(error = TRUE, {
    filter(df, pick(x, y)$x)
  })
  expect_snapshot(error = TRUE, {
    filter(df, pick_wrapper(x, y)$x)
  })
})

# ------------------------------------------------------------------------------
# pick() + group_by()

test_that("`pick()` can be used inside `group_by()` wrappers", {
  df <- tibble(a = 1:3, b = 2:4, c = 3:5)

  tidyselect_group_by <- function(data, groups) {
    group_by(data, pick({{ groups }}))
  }
  expect_identical(
    tidyselect_group_by(df, c(a, c)),
    group_by(df, a, c)
  )

  tidyselect_group_by_wrapper <- function(data, groups) {
    group_by(data, pick_wrapper({{ groups }}))
  }
  expect_identical(
    tidyselect_group_by_wrapper(df, c(a, c)),
    group_by(df, a, c)
  )
})

# ------------------------------------------------------------------------------
# expand_pick()

test_that("`pick()` doesn't expand across anonymous function boundaries", {
  df <- tibble(x = 1, y = 2)
  by <- compute_by(by = NULL, data = df, error_call = current_env())
  mask <- DataMask$new(df, by, verb = "mutate", error_call = current_env())

  # With inline `function() { }` calls (this also handles native R anonymous functions)
  quo <- dplyr_quosures(z = function() pick(y, x))$z
  expect_identical(expand_pick(quo, mask), quo)

  # With `~` anonymous functions
  quos <- dplyr_quosures(z = ~ pick(y, x))$z
  expect_identical(expand_pick(quo, mask), quo)
})

test_that("`pick()` expands embedded quosures", {
  df <- tibble(x = 1, y = 2)
  by <- compute_by(by = NULL, data = df, error_call = current_env())
  mask <- DataMask$new(df, by, verb = "mutate", error_call = current_env())

  wrapper <- function(x) {
    dplyr_quosures(z = dense_rank({{x}}))
  }
  quo <- wrapper(pick(x, y))$z

  out <- expand_pick(quo, mask)

  expect_identical(
    quo_get_expr(quo_get_expr(out)[[2L]]),
    expr(asNamespace("dplyr")$dplyr_pick_tibble(x = x, y = y))
  )
})
