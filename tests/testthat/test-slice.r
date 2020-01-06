context("slice")

test_that("slice handles numeric input (#226)", {
  g <- mtcars %>% arrange(cyl) %>% group_by(cyl)
  res <- g %>% slice(1)
  expect_equal(nrow(res), 3)
  expect_equal(res, g %>% filter(row_number() == 1L))

  expect_equal(
    mtcars %>% slice(1),
    mtcars %>% filter(row_number() == 1L)
  )
})

test_that("slice silently ignores out of range values (#226)", {
  expect_equal(slice(mtcars, c(2, 100)), slice(mtcars, 2))

  g <- group_by(mtcars, cyl)
  expect_equal(slice(g, c(2, 100)), slice(g, 2))
})

test_that("slice works with negative indices", {
  res <- slice(mtcars, -(1:2))
  exp <- tail(mtcars, -2)
  expect_equivalent(res, exp)
})

test_that("slice forbids positive and negative together", {
  expect_error(
    mtcars %>% slice(c(-1, 2)),
    class = "dplyr_slice_ambiguous"
  )
  expect_error(
    mtcars %>% slice(c(2:3, -1)),
    class = "dplyr_slice_ambiguous"
  )
})

test_that("slice works with grouped data", {
  g <- mtcars %>% arrange(cyl) %>% group_by(cyl)

  res <- slice(g, 1:2)
  exp <- filter(g, row_number() < 3)
  expect_equal(res, exp)

  res <- slice(g, -(1:2))
  exp <- filter(g, row_number() >= 3)
  expect_equal(res, exp)

  g <- group_by(data.frame(x = c(1, 1, 2, 2, 2)), x)
  expect_equal(group_keys(slice(g, 3, .preserve = TRUE))$x, c(1, 2))
  expect_equal(group_keys(slice(g, 3, .preserve = FALSE))$x, 2)
})

test_that("slice gives correct rows (#649)", {
  a <- tibble(value = paste0("row", 1:10))
  expect_equal(slice(a, 1:3)$value, paste0("row", 1:3))
  expect_equal(slice(a, c(4, 6, 9))$value, paste0("row", c(4, 6, 9)))

  a <- tibble(
    value = paste0("row", 1:10),
    group = rep(1:2, each = 5)
  ) %>%
    group_by(group)

  expect_equal(slice(a, 1:3)$value, paste0("row", c(1:3, 6:8)))
  expect_equal(slice(a, c(2, 4))$value, paste0("row", c(2, 4, 7, 9)))
})

test_that("slice handles NA (#1235)", {
  df <- tibble(x = 1:3)
  expect_equal(nrow(slice(df, NA_integer_)), 0L)
  expect_equal(nrow(slice(df, c(1L, NA_integer_))), 1L)
  expect_equal(nrow(slice(df, c(-1L, NA_integer_))), 2L)

  df <- tibble(x = 1:4, g = rep(1:2, 2)) %>% group_by(g)
  expect_equal(nrow(slice(df, c(1, NA))), 2)
  expect_equal(nrow(slice(df, c(-1, NA))), 2)
})

test_that("slice handles logical NA (#3970)", {
  df <- tibble(x = 1:3)
  expect_equal(nrow(slice(df, NA)), 0L)
  expect_error(
    slice(df, TRUE),
    class = "dplyr_slice_incompatible"
  )
  expect_error(
    slice(df, FALSE),
    class = "dplyr_slice_incompatible"
  )
})

test_that("slice handles empty data frames (#1219)", {
  df <- data.frame(x = numeric())
  res <- df %>% slice(1:3)
  expect_equal(nrow(res), 0L)
  expect_equal(names(res), "x")
})

test_that("slice works fine if n > nrow(df) (#1269)", {
  by_slice <- mtcars %>% arrange(cyl) %>%  group_by(cyl)
  slice_res <- by_slice  %>% slice(8)
  filter_res <- by_slice %>% group_by(cyl) %>% filter(row_number() == 8)
  expect_equal(slice_res, filter_res)
})

test_that("slice strips grouped indices (#1405)", {
  res <- mtcars %>% group_by(cyl) %>% slice(1) %>% mutate(mpgplus = mpg + 1)
  expect_equal(nrow(res), 3L)
  expect_equal(group_rows(res), list_of(1L, 2L, 3L))
})

test_that("slice works with zero-column data frames (#2490)", {
  expect_equal(
    tibble(a = 1:3) %>% select(-a) %>% slice(1) %>% nrow(),
    1L
  )
})

test_that("slice correctly computes positive indices from negative indices (#3073)", {
  x <- tibble(y = 1:10)
  expect_identical(slice(x, -10:-30), tibble(y = 1:9))
})

test_that("slice handles raw matrices", {
  df <- tibble(a = 1:4, b = matrix(as.raw(1:8), ncol = 2))
  expect_identical(
    slice(df, 1:2)$b,
    matrix(as.raw(c(1, 2, 5, 6)), ncol = 2)
  )
})

test_that("slice on ungrouped data.frame (not tibble) does not enforce tibble", {
  expect_equal(class(slice(mtcars, 2)), "data.frame")
  expect_equal(class(slice(mtcars, -2)), "data.frame")
  expect_equal(class(slice(mtcars, NA)), "data.frame")
})

test_that("slice skips 0 (#3313)", {
  d <- tibble(x = 1:5, y = LETTERS[1:5], g = 1)
  expect_identical(slice(d, 0), slice(d, integer(0)))
  expect_identical(slice(d, c(0, 1)), slice(d, 1))
  expect_identical(slice(d, c(0, 1, 2)), slice(d, c(1, 2)))

  expect_identical(slice(d, c(-1, 0)), slice(d, -1))
  expect_identical(slice(d, c(0, -1)), slice(d, -1))

  d <- group_by(d, g)
  expect_identical(slice(d, 0), slice(d, integer(0)))
  expect_identical(slice(d, c(0, 1)), slice(d, 1))
  expect_identical(slice(d, c(0, 1, 2)), slice(d, c(1, 2)))

  expect_identical(slice(d, c(-1, 0)), slice(d, -1))
  expect_identical(slice(d, c(0, -1)), slice(d, -1))
})

test_that("slice accepts ... (#3804)", {
  expect_equal(slice(mtcars, 1, 2), slice(mtcars, 1:2))
  expect_equal(slice(mtcars, 1, n()), slice(mtcars, c(1, nrow(mtcars))))

  g <- mtcars %>% group_by(cyl)
  expect_equal(slice(g, 1, n()), slice(g, c(1, n())))
})

test_that("slice does not evaluate the expression in empty groups (#1438)", {
  res <- mtcars %>%
    group_by(cyl) %>%
    filter(cyl==6) %>%
    slice(1:2)
  expect_equal(nrow(res), 2L)

  expect_condition(
    res <- mtcars %>% group_by(cyl) %>% filter(cyl==6) %>% sample_n(size=3),
    NA
  )
  expect_equal(nrow(res), 3L)
})

test_that("slice() handles matrix and data frame columns (#3630)", {
  df <- tibble(
    x = 1:2,
    y = matrix(1:4, ncol = 2),
    z = data.frame(A = 1:2, B = 3:4)
  )
  expect_equal(slice(df, 1), df[1, ])
  expect_equal(slice(df, 1), df[1, ])
  expect_equal(slice(df, 1), df[1, ])

  gdf <- group_by(df, x)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)

  gdf <- group_by(df, y)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)

  gdf <- group_by(df, z)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)
})


# Slice variants ----------------------------------------------------------

test_that("functions silently truncate results", {
  df <- data.frame(x = 1:5)

  expect_equal(df %>% slice_head(n = 6) %>% nrow(), 5)
  expect_equal(df %>% slice_tail(n = 6) %>% nrow(), 5)
  expect_equal(df %>% slice_sample(n = 6) %>% nrow(), 5)
  expect_equal(df %>% slice_min(x, n = 6) %>% nrow(), 5)
  expect_equal(df %>% slice_max(x, n = 6) %>% nrow(), 5)
})

test_that("min and max return ties by default", {
  df <- data.frame(x = c(1, 1, 1, 2, 2))
  expect_equal(df %>% slice_min(x, n = 1) %>% nrow(), 3)
  expect_equal(df %>% slice_max(x, n = 1) %>% nrow(), 2)

  expect_equal(df %>% slice_min(x, n = 1, with_ties = FALSE) %>% nrow(), 1)
  expect_equal(df %>% slice_max(x, n = 1, with_ties = FALSE) %>% nrow(), 1)
})

test_that("min and max reorder results", {
  df <- data.frame(id = 1:4, x = c(2, 3, 1, 2))

  expect_equal(df %>% slice_min(x, n = 2) %>% pull(id), c(3, 1, 4))
  expect_equal(df %>% slice_min(x, n = 2, with_ties = FALSE) %>% pull(id), c(3, 1))
  expect_equal(df %>% slice_max(x, n = 2) %>% pull(id), c(2, 1, 4))
  expect_equal(df %>% slice_max(x, n = 2, with_ties = FALSE) %>% pull(id), c(2, 1))
})

test_that("arguments to sample are passed along", {
  df <- data.frame(x = 1:100, wt = c(1, rep(0, 99)))

  expect_equal(df %>% slice_sample(n = 1, weight_by = wt) %>% pull(x), 1)
  expect_equal(df %>% slice_sample(n = 2, weight_by = wt, replace = TRUE) %>% pull(x), c(1, 1))
})

test_that("n and prop are carefully validated", {
  expect_error(check_slice_size(), "exactly one")
  expect_error(check_slice_size(n = 1, prop = 1), "exactly one")
  expect_error(check_slice_size(n = "a"), "single number")
  expect_error(check_slice_size(prop = "a"), "single number")
  expect_error(check_slice_size(n = -1), "positive number")
  expect_error(check_slice_size(prop = -1), "positive number")
})
