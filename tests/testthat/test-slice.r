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

test_that("slice works with 0 args", {
  expect_equivalent(slice(mtcars), mtcars)
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
})

test_that("slice works with grouped data", {
  g <- group_by(mtcars, cyl)

  res <- slice(g, 1:2)
  exp <- filter(g, row_number() < 3)
  expect_equal(res, exp)

  res <- slice(g, -(1:2))
  exp <- filter(g, row_number() >= 3)
  expect_equal(res, exp)

})

test_that("slice gives correct rows (#649)", {
  a <- data_frame(value = paste0("row", 1:10))
  expect_equal(slice(a, 1:3)$value, paste0("row", 1:3))
  expect_equal(slice(a, c(4, 6, 9))$value, paste0("row", c(4, 6, 9)))

  a <- data_frame(
    value = paste0("row", 1:10),
    group = rep(1:2, each = 5)
  ) %>%
    group_by(group)

  expect_equal(slice(a, 1:3)$value, paste0("row", c(1:3, 6:8)))
  expect_equal(slice(a, c(2, 4))$value, paste0("row", c(2, 4, 7, 9)))
})

test_that("slice handles NA (#1235)", {
  df <- data_frame(x = 1:3)
  expect_equal(nrow(slice(df, NA_integer_)), 0L)
  expect_equal(nrow(slice(df, c(1L, NA_integer_))), 1L)
  expect_equal(nrow(slice(df, c(-1L, NA_integer_))), 2L)

  df <- data_frame(x = 1:4, g = rep(1:2, 2)) %>% group_by(g)
  expect_equal(nrow(slice(df, NA)), 0L)
  expect_equal(nrow(slice(df, c(1, NA))), 2)
  expect_equal(nrow(slice(df, c(-1, NA))), 2)

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
  expect_equal(attr(res, "indices"), as.list(0:2))
})

test_that("slice works with zero-column data frames (#2490)", {
  expect_equal(
    data_frame(a = 1:3) %>% select(-a) %>% slice(1) %>% nrow,
    1L
  )
})

test_that("slice works under gctorture2", {
  x <- tibble(y = 1:10)
  with_gctorture2(999, x2 <- slice(x, 1:10))
  expect_identical(x, x2)
})
