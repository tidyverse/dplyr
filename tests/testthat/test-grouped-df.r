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

test_that("group order is maintained in grouped-df methods (#5040)", {
  gdf <- group_by(mtcars, cyl, am, vs)

  x <- gdf[0,]
  expect_identical(group_vars(x), group_vars(gdf))

  x <- gdf
  x$am <- 1
  expect_identical(group_vars(x), group_vars(gdf))

  x <- gdf
  x["am"] <- 1
  expect_identical(group_vars(x), group_vars(gdf))

  x <- gdf
  x[["am"]] <- 1
  expect_identical(group_vars(x), group_vars(gdf))

  x <- gdf
  names <- names(x)
  names[9] <- "am2"
  names(x) <- names
  expect_identical(group_vars(x), group_vars(group_by(x, cyl, am2, vs)))
})


# validate ----------------------------------------------------------------

test_that("validate_grouped_df() gives useful errors", {
  df1 <- group_by(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
  groups <- attr(df1, "groups")
  groups[[2]] <- 1:2
  attr(df1, "groups") <- groups

  df2 <- group_by(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
  groups <- attr(df2, "groups")
  names(groups) <- c("g", "not.rows")
  attr(df2, "groups") <- groups

  df3 <- df2
  attr(df3, "groups") <- tibble()

  df4 <- df3
  attr(df4, "groups") <- NA

  df5 <- tibble(x = 1:4, g = rep(1:2, each = 2))
  attr(df5, "vars") <- "g"
  attr(df5, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")

  df6 <- new_grouped_df(
    tibble(x = 1:10),
    groups = tibble(".rows" := list(1:5, -1L))
  )
  df7 <- df6
  attr(df7, "groups")$.rows <- list(11L)

  df8 <- df6
  attr(df8, "groups")$.rows <- list(0L)

  df10 <- df6
  attr(df10, "groups") <- tibble()

  df11 <- df6
  attr(df11, "groups") <- NULL

  verify_output(test_path("test-grouped-df-validate.txt"), {
    "# Invalid `groups` attribute"
    validate_grouped_df(df1)
    validate_grouped_df(df2)
    validate_grouped_df(df3)
    validate_grouped_df(df4)

    "# Older style grouped_df"
    validate_grouped_df(df5)

    "# validate_grouped_df()"
    validate_grouped_df(df6, check_bounds = TRUE)
    validate_grouped_df(df7, check_bounds = TRUE)
    validate_grouped_df(df8, check_bounds = TRUE)
    validate_grouped_df(df10)
    validate_grouped_df(df11)

    "# new_grouped_df()"
    new_grouped_df(
      tibble(x = 1:10),
      tibble(other = list(1:2))
    )
  })
})

# compute_group ----------------------------------------------------------

test_that("helper gives meaningful error messages", {
  verify_output(test_path("test-grouped-df-errors.txt"), {
    grouped_df(data.frame(x = 1), "y", FALSE)
    grouped_df(data.frame(x = 1), 1)
  })
})
