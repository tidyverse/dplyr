context("Sample")

# Basic behaviour -------------------------------------------------------------

test_that("sample preserves class", {
  expect_is(sample_n(mtcars, 1), "data.frame")
  expect_is(sample_n(tbl_df(mtcars), 1), "tbl_df")

  expect_is(sample_frac(mtcars, 1), "data.frame")
  expect_is(sample_frac(tbl_df(mtcars), 1), "tbl_df")
})

# Ungrouped --------------------------------------------------------------------

df <- data.frame(
  x = 1:2,
  y = c(0, 1)
)

test_that("sample respects weight", {
  # error message from base R
  expect_error(sample_n(df, 2, weight = y))
  expect_equal(sample_n(df, 1, weight = y)$x, 2)

  expect_error(
    sample_frac(df, 2),
    "`size` of sampled fraction must be less or equal to one, set `replace` = TRUE to use sampling with replacement",
    fixed = TRUE
  )
  expect_error(
    sample_frac(df %>% group_by(y), 2),
    "`size` of sampled fraction must be less or equal to one, set `replace` = TRUE to use sampling with replacement",
    fixed = TRUE
  )
  # error message from base R
  expect_error(sample_frac(df, 1, weight = y))
  expect_equal(sample_frac(df, 0.5, weight = y)$x, 2)
})

test_that("sample_* error message", {
  expect_error(
    check_weight(letters[1:2], 2),
    "`weight` must be a numeric, not a character vector",
    fixed = TRUE
  )
  expect_error(
    check_weight(-1:-2, 2),
    "`weight` must be a vector with all values nonnegative, not -1",
    fixed = TRUE
  )
  expect_error(
    check_weight(letters, 2),
    "`weight` must be a numeric, not a character vector"
  )
})

test_that("sample gives informative error for unknown type", {
  expect_error(
    sample_n(list()),
    "`tbl` must be a data frame, not a list",
    fixed = TRUE
  )

  expect_error(
    sample_frac(list()),
    "`tbl` must be a data frame, not a list",
    fixed = TRUE
  )
})

# Grouped ----------------------------------------------------------------------

test_that("sampling grouped tbl samples each group", {
  sampled <- mtcars %>% group_by(cyl) %>% sample_n(2)
  expect_is(sampled, "grouped_df")
  expect_groups(sampled, "cyl")
  expect_equal(nrow(sampled), 6)
  expect_equal(map_int(group_rows(sampled), length), c(2,2,2))
})

test_that("can't sample more values than obs (without replacement)", {
  by_cyl <- mtcars %>% group_by(cyl)
  expect_error(
    sample_n(by_cyl, 10),
    "`size` must be less or equal than 7 (size of data), set `replace` = TRUE to use sampling with replacement",
    fixed = TRUE
  )
})

test_that("grouped sample respects weight", {
  df2 <- tibble(
    x = rep(1:2, 100),
    y = rep(c(0, 1), 100),
    g = rep(1:2, each = 100)
  )

  grp <- df2 %>% group_by(g)

  # error message from base R
  expect_error(sample_n(grp, nrow(df2) / 2, weight = y))
  expect_equal(sample_n(grp, 1, weight = y)$x, c(2, 2))

  # error message from base R
  expect_error(sample_frac(grp, 1, weight = y))
  expect_equal(sample_frac(grp, 0.5, weight = y)$x, rep(2, nrow(df2) / 2))
})

test_that("grouped sample accepts NULL weight from variable (for saeSim)", {
  df <- tibble(
    x = rep(1:2, 10),
    y = rep(c(0, 1), 10),
    g = rep(1:2, each = 10)
  )

  weight <- NULL

  expect_error(sample_n(df, nrow(df), weight = weight), NA)
  expect_error(sample_frac(df, weight = weight), NA)

  grp <- df %>% group_by(g)

  expect_error(sample_n(grp, nrow(df) / 2, weight = weight), NA)
  expect_error(sample_frac(grp, weight = weight), NA)
})

test_that("sample_n and sample_frac can call n() (#3413)", {
  df <- tibble(
    x = rep(1:2, 10),
    y = rep(c(0, 1), 10),
    g = rep(1:2, each = 10)
  )
  gdf <- group_by(df, g)

  expect_equal(nrow(sample_n(df, n())), nrow(df))
  expect_equal(nrow(sample_n(gdf, n())), nrow(gdf))

  expect_equal(nrow(sample_n(df, n() - 2L)), nrow(df) - 2)
  expect_equal(nrow(sample_n(gdf, n() - 2L)), nrow(df) - 4)
})

test_that("sample_n and sample_frac handles lazy grouped data frames (#3380)", {
  df1 <- data.frame(x = 1:10, y = rep(1:2, each=5))
  df2 <- data.frame(x = 6:15, z = 1:10)
  res <- df1 %>% group_by(y) %>% anti_join(df2, by="x") %>% sample_n(1)
  expect_equal(nrow(res), 1L)

  res <- df1 %>% group_by(y) %>% anti_join(df2, by="x") %>% sample_frac(0.2)
  expect_equal(nrow(res), 1L)
})
