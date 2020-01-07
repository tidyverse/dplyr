test_that("can selectively ungroup", {
  gf <- group_by(tibble(x = 1, y = 2), x, y)

  expect_equal(gf %>% ungroup() %>% group_vars(), character())
  expect_equal(gf %>% ungroup(everything()) %>% group_vars(), character())
  expect_equal(gf %>% ungroup(x) %>% group_vars(), "y")
  expect_error(
    gf %>% ungroup(z) %>% group_vars(),
    class = "tidyselect_error_index_oob_names"
  )
})


test_that("[ method can remove grouping vars", {
  df <- tibble(x = 1, y = 2, z = 3)
  gf <- group_by(df, x, y)

  expect_equal(gf, gf)
  expect_equal(gf[1], group_by(df[1], x))
  expect_equal(gf[3], df[3])
})

test_that("[ supports drop=TRUE (#3714)", {
  df <- tibble(x = 1, y = 2)
  gf <- group_by(df, x)

  expect_type(gf[, "y", drop = TRUE], "double")
  expect_s3_class(gf[, c("x", "y"), drop = TRUE], "grouped_df")
})

test_that("$<-, [[<-, and [<- update grouping data if needed", {
  df <- tibble(x = 1, y = 2)
  gf <- group_by(df, x)

  expect_equal(group_data(`$<-`(gf, "x", 2))$x, 2)
  expect_equal(group_data(`$<-`(gf, "y", 2))$x, 1)

  expect_equal(group_data({gf2 <- gf; gf2[[1]] <- 3; gf2})$x, 3)
  expect_equal(group_data(`[<-`(gf, 1, "x", 4))$x, 4)
})

test_that("can remove grouping cols with subset assignment", {
  df <- tibble(x = 1, y = 2)
  gf1 <- gf2 <- gf3 <- group_by(df, x, y)

  gf1$x <- NULL
  gf2[["x"]] <- NULL
  gf3[, "x"] <- NULL

  expect_named(group_data(gf1), c("y", ".rows"))
  expect_named(group_data(gf2), c("y", ".rows"))
  expect_named(group_data(gf3), c("y", ".rows"))
})
