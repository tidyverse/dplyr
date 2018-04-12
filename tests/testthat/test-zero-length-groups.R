context("zero length groups")

df <- data_frame(f = factor( c(1,1,2,2), levels = 1:3), x = c(1,2,1,4)) %>%
  group_by( f, drop = FALSE )

test_that("filter and slice keep zero length groups", {
  expect_equal( group_size(filter(df, f == 1)), c(2, 0, 0) )
  expect_equal( group_size(slice(df, 1)), c(1, 1, 0) )
})

test_that("mutate keeps zero length groups", {
  expect_equal( group_size(mutate(df, z = 2)), c(2, 2, 0) )
})

test_that("summarise returns a row for zero length groups", {
  expect_equal( nrow(summarise(df, z = n())), 3L)
})

test_that("arrange keeps zero length groups",{
  expect_equal( group_size(arrange(df)), c(2, 2, 0) )
})

test_that("bind_rows respect the drop attribute of grouped df",{
  df <- data_frame(f = factor( c(1,1,2,2), levels = 1:3), x = c(1,2,1,4))
  g <- group_by(df, f, drop = FALSE)

  gg <- bind_rows(g,g)
  expect_equal(group_size(gg), c(4L,4L,0L))
})

test_that("joins respect zero length groups", {
  df1 <- data_frame(f = factor( c(1,1,2,2), levels = 1:3), x = c(1,2,1,4)) %>%
    group_by(f, drop = FALSE)

  df2 <- data_frame(f = factor( c(2,2,3,3), levels = 1:3), y = c(1,2,3,4)) %>%
    group_by(f, drop = FALSE)

  expect_equal(group_size(left_join( df1, df2, by = "f")),  c(2,4,0))
  expect_equal(group_size(right_join( df1, df2, by = "f")),  c(0,4,2))
  expect_equal(group_size(full_join( df1, df2, by = "f")),  c(2,4,2))
  expect_equal(group_size(anti_join( df1, df2, by = "f")),  c(2,0,0))
  expect_equal(group_size(inner_join( df1, df2, by = "f")),  c(0,4,0))
})
