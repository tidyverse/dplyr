context("slice")

test_that("slice handles numeric input (#226)", {
  g <- mtcars %>% group_by(cyl)
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
  expect_equal(names(res), names(exp))
  for (col in names(res)) {
    expect_equal(res[[col]], exp[[col]])
  }
})

test_that("slice forbids positive and negative together", {
  expect_error(
    mtcars %>% slice(c(-1, 2)),
    "Found 1 positive indices and 1 negative indices",
    fixed = TRUE
  )
  expect_error(
    mtcars %>% slice(c(2:3, -1)),
    "Found 2 positive indices and 1 negative indices",
    fixed = TRUE
  )
})

test_that("slice works with grouped data", {
  g <- group_by(mtcars, cyl)

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
  expect_error(slice(df, TRUE))
  expect_error(slice(df, FALSE))
})

test_that("slice handles empty data frames (#1219)", {
  df <- data.frame(x = numeric())
  res <- df %>% slice(1:3)
  expect_equal(nrow(res), 0L)
  expect_equal(names(res), "x")
})

test_that("slice works fine if n > nrow(df) (#1269)", {
  slice_res <- mtcars %>% group_by(cyl) %>% slice(8)
  filter_res <- mtcars %>% group_by(cyl) %>% filter(row_number() == 8)
  expect_equal(slice_res, filter_res)
})

test_that("slice strips grouped indices (#1405)", {
  res <- mtcars %>% group_by(cyl) %>% slice(1) %>% mutate(mpgplus = mpg + 1)
  expect_equal(nrow(res), 3L)
  expect_equal(group_rows(res), as.list(1:3))
})

test_that("slice works with zero-column data frames (#2490)", {
  expect_equal(
    tibble(a = 1:3) %>% select(-a) %>% slice(1) %>% nrow(),
    1L
  )
})

test_that("slice works under gctorture2", {
  x <- tibble(y = 1:10)
  with_gctorture2(999, x2 <- slice(x, 1:10))
  expect_identical(x, x2)
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

test_that("slice is not confused about dense groups (#3753)",{
  df <- tibble(row = 1:3)
  expect_equal(slice(df, c(2,1,3))$row, c(2L,1L,3L))
  expect_equal(slice(df, c(1,1,1))$row, rep(1L, 3))
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

test_that("column_subset() falls back to R indexing on esoteric data types (#4128)", {
  res <- slice(tibble::enframe(formals(rnorm)), 2:3)
  expect_identical(res, tibble(name = c("mean", "sd"), value = list(0, 1)))
})
