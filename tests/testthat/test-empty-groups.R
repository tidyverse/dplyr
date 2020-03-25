context("empty groups")

df <- tibble(
  e = 1,
  f = factor(c(1, 1, 2, 2), levels = 1:3),
  g = c(1, 1, 2, 2),
  x = c(1, 2, 1, 4)
) %>%
  group_by(e, f, g, .drop = FALSE)

test_that("filter and slice keep zero length groups", {
  expect_equal(group_size(filter(df, f == 1)), c(2, 0, 0) )
  expect_equal(group_size(slice(df, 1)), c(1, 1, 0) )
})

test_that("filtering and slicing retains labels for zero length groups", {
  expect_equal(
    ungroup(count(filter(df, f == 1))),
    tibble(
      e = 1,
      f = factor(1:3),
      g = c(1, 2, NA),
      n = c(2L, 0L, 0L)
    )
  )

  expect_equal(
    ungroup(count(slice(df, 1))),
    tibble(
      e = 1,
      f = factor(1:3),
      g = c(1, 2, NA),
      n = c(1L, 1L, 0L)
    )
  )
})

test_that("mutate keeps zero length groups", {
  expect_equal( group_size(mutate(df, z = 2)), c(2, 2, 0) )
})

test_that("summarise returns a row for zero length groups", {
  expect_equal( nrow(summarise(df, z = n())), 3L)
})

test_that("arrange keeps zero length groups",{
  expect_equal( group_size(arrange(df)), c(2, 2, 0) )
  expect_equal( group_size(arrange(df, x)), c(2, 2, 0) )
})

test_that("bind_rows respect the drop attribute of grouped df",{
  gg <- bind_rows(df, df)
  expect_equal(group_size(gg), c(4L,4L,0L))
})

test_that("joins respect zero length groups", {
  df1 <- tibble(f = factor( c(1,1,2,2), levels = 1:3), x = c(1,2,1,4)) %>%
    group_by(f)

  df2 <- tibble(f = factor( c(2,2,3,3), levels = 1:3), y = c(1,2,3,4)) %>%
    group_by(f)

  expect_equal(group_size(left_join( df1, df2, by = "f")),  c(2,4))
  expect_equal(group_size(right_join( df1, df2, by = "f")),  c(4,2))
  expect_equal(group_size(full_join( df1, df2, by = "f")),  c(2,4,2))
  expect_equal(group_size(anti_join( df1, df2, by = "f")),  c(2))
  expect_equal(group_size(inner_join( df1, df2, by = "f")),  c(4))


  df1 <- tibble(f = factor( c(1,1,2,2), levels = 1:3), x = c(1,2,1,4)) %>%
    group_by(f, .drop = FALSE)
  df2 <- tibble(f = factor( c(2,2,3,3), levels = 1:3), y = c(1,2,3,4)) %>%
    group_by(f, .drop = FALSE)

  expect_equal(group_size(left_join( df1, df2, by = "f")),  c(2,4,0))
  expect_equal(group_size(right_join( df1, df2, by = "f")),  c(0,4,2))
  expect_equal(group_size(full_join( df1, df2, by = "f")),  c(2,4,2))
  expect_equal(group_size(anti_join( df1, df2, by = "f")),  c(2,0,0))
  expect_equal(group_size(inner_join( df1, df2, by = "f")),  c(0,4,0))
})

test_that("n_groups respects zero-length groups (#341)", {
  df <- tibble(x = factor(1:3, levels = 1:4)) %>% group_by(x, .drop = FALSE)
  expect_equal(n_groups(df), 4)
})

test_that("summarise respects zero-length groups (#341)", {
  df <- tibble(x = factor(rep(1:3, each = 10), levels = 1:4))

  out <- df %>%
    group_by(x, .drop = FALSE) %>%
    summarise(n = n())

  expect_equal(out$n, c(10L, 10L, 10L, 0L))
})
