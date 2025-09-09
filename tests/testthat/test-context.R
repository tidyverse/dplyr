test_that("cur_group() works", {
  df <- tibble(g = 1, x = 1)
  gf <- group_by(df, g)

  expect_equal(
    df |> summarise(key = list(cur_group())) |> pull(key),
    list(tibble(.rows = 1L))
  )
  expect_equal(
    gf |> summarise(key = list(cur_group())) |> pull(key),
    list(tibble(g = 1))
  )
})

test_that("cur_group() works with empty grouped data frame (#6304)", {
  df <- tibble(x = integer())
  gdf <- group_by(df, x)

  out <- mutate(df, y = cur_group())
  expect_identical(out$y, tibble())

  out <- mutate(gdf, y = cur_group())
  expect_identical(out$y, tibble(x = integer()))
})

test_that("cur_group_idx() gives unique id", {
  df <- tibble(x = c("b", "a", "b"))
  gf <- group_by(df, x)

  expect_equal(
    summarise(gf, id = cur_group_id()),
    tibble(x = c("a", "b"), id = 1:2)
  )
  expect_equal(
    mutate(gf, id = cur_group_id()),
    group_by(tibble(x = df$x, id = c(2, 1, 2)), x)
  )
})

test_that("cur_group_rows() retrieves row position in original data", {
  df <- tibble(x = c("b", "a", "b"), y = 1:3)
  gf <- group_by(df, x)

  expect_equal(
    df |> summarise(x = list(cur_group_rows())) |> pull(),
    list(1:3)
  )

  expect_equal(
    gf |> summarise(x = list(cur_group_rows())) |> pull(),
    list(2L, c(1L, 3L))
  )
})

test_that("give useful error messages when not applicable", {
  expect_snapshot({
    (expect_error(n()))

    (expect_error(cur_column()))
    (expect_error(cur_group()))
    (expect_error(cur_group_id()))
    (expect_error(cur_group_rows()))
  })
})

test_that("group labels are correctly formatted", {
  expect_snapshot({
    group_labels_details(c("a" = 1))
    group_labels_details(c("a" = 1, "b" = 2))
  })
})
