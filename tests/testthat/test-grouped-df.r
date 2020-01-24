test_that("new_grouped_df can create alternative grouping structures (#3837)", {
  tbl <- new_grouped_df(
    tibble(x = rnorm(10)),
    groups = tibble(".rows" := replicate(5, sample(1:10, replace = TRUE), simplify = FALSE))
  )
  res <- summarise(tbl, x = mean(x))
  expect_equal(nrow(res), 5L)
})

test_that("new_grouped_df does not have rownames (#4173)", {
  tbl <- new_grouped_df(
    tibble(x = rnorm(10)),
    groups = tibble(".rows" := replicate(5, sample(1:10, replace = TRUE), simplify = FALSE))
  )
  expect_false(tibble::has_rownames(tbl))
})

test_that("[ method can remove grouping vars", {
  df <- tibble(x = 1, y = 2, z = 3)
  gf <- group_by(df, x, y)

  expect_equal(gf, gf)
  expect_equal(gf[1], group_by(df[1], x))
  expect_equal(gf[3], df[3])
})

test_that("[ method reuses group_data() if possible", {
  df <- tibble(x = 1, y = 2, z = 3)
  gf <- group_by(df, x, y)

  expect_reference(group_data(gf), group_data(gf[1:2]))
  expect_reference(group_data(gf), group_data(gf[, 1:2]))
})

test_that("[ supports drop=TRUE (#3714)", {
  df <- tibble(x = 1, y = 2)
  gf <- group_by(df, x)

  expect_type(gf[, "y", drop = TRUE], "double")
  expect_s3_class(gf[, c("x", "y"), drop = TRUE], "tbl_df")
})

test_that("$<-, [[<-, and [<- update grouping data if needed", {
  df <- tibble(x = 1, y = 2)
  gf <- group_by(df, x)

  # value has to be past the ellipsis in $<-()
  expect_equal(group_data(`$<-`(gf, "x", value = 2))$x, 2)
  expect_equal(group_data(`$<-`(gf, "y", value = 2))$x, 1)

  expect_equal(group_data({gf2 <- gf; gf2[[1]] <- 3; gf2})$x, 3)
  expect_equal(group_data(`[<-`(gf, 1, "x", value = 4))$x, 4)
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

test_that("names<- updates grouping data", {
  df <- tibble(x = 1, y = 2, z = 3)
  gf <- group_by(df, x, y)

  names(gf) <- c("z1", "z2", "z3")
  expect_named(group_data(gf), c("z1", "z2", ".rows"))

  names(gf)[1] <- c("Z1")
  expect_named(group_data(gf), c("Z1", "z2", ".rows"))
})

test_that("names<- doesn't modify group data if not necessary", {
  df <- tibble(x = 1, y = 2)
  gf1 <- gf2 <- group_by(df, x)
  expect_reference(group_data(gf1), group_data(gf2))

  names(gf1) <- c("x", "Y")
  expect_reference(group_data(gf1), group_data(gf2))
})


# compute_group ----------------------------------------------------------

test_that("helper gives meaningful error messages", {
  verify_output(test_path("test-grouped-df-errors.txt"), {
    grouped_df(data.frame(x = 1), "y", FALSE)
    grouped_df(data.frame(x = 1), 1)
  })
})

test_that("compute_group warns about implicit missing values", {
  df <- tibble(x = 1:3, f = factor(c("a", "b", NA)))
  expect_warning(g <- compute_groups(df, "f"), "Factor `f` contains implicit NA")
  expect_equal(as.list(g$.rows), list(1L, 2L, 3L))
})
