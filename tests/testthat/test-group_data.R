context("group_data")

test_that("group_rows works for 3 most important subclasses (#3489)", {
  df <- data.frame(x=c(1,1,2,2))
  expect_equal(group_rows(df), list(1:4))
  expect_equal(group_rows(group_by(df,x)), list(1:2, 3:4))
  expect_equal(group_rows(rowwise(df)), as.list(1:4))
})

test_that("group_data() returns a tidy tibble (#3489)", {
  df <- tibble(x = c(1,1,2,2))

  expect_identical(
    group_data(df),
    tibble(".rows" := list(1:4))
  )

  expect_identical(
    group_by(df,x) %>% group_data(),
    tibble::new_tibble(list(x = c(1,2), .rows = list(1:2, 3:4)), .drop = TRUE, nrow = 2L)
  )

  expect_identical(
    rowwise(df) %>% group_data(),
    tibble(".rows" := as.list(1:4))
  )
})

test_that("group_rows and group_data work with 0 rows data frames (#3489)", {
  df <- tibble(x=integer())
  expect_identical(group_rows(df), list(integer()))
  expect_identical(group_rows(rowwise(df)), list())
  expect_identical(group_rows(group_by(df, x)), list())

  expect_identical(group_data(df), tibble(".rows" := list(integer())))
  expect_identical(group_data(rowwise(df)), tibble(".rows" := list()))
  expect_identical(
    group_data(group_by(df, x)),
    tibble::new_tibble(list(x = integer(), .rows = list()), .drop = TRUE, nrow = 0L)
  )
})

test_that("GroupDataFrame checks the structure of the groups attribute", {
  df <- group_by(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
  groups <- attr(df, "groups")
  groups[[2]] <- 1:2
  attr(df, "groups") <- groups
  expect_error(group_data(df), "is a corrupt grouped_df")

  df <- group_by(tibble(x = 1:4, g = rep(1:2, each = 2)), g)
  groups <- attr(df, "groups")
  names(groups) <- c("g", "not.rows")
  attr(df, "groups") <- groups
  expect_error(group_data(df), "is a corrupt grouped_df")

  attr(df, "groups") <- tibble()
  expect_error(group_data(df), "is a corrupt grouped_df")

  attr(df, "groups") <- NA
  expect_error(group_data(df), "is a corrupt grouped_df")
})

test_that("GroupedDataFrame is compatible with older style grouped_df (#3604)", {
  df <- tibble(x = 1:4, g = rep(1:2, each = 2))
  attr(df, "vars") <- "g"
  attr(df, "class") <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  expect_equal(expect_warning(group_rows(df)), list(1:2, 3:4))

  df <- structure(
    data.frame(x=1),
    class = c("grouped_df", "tbl_df", "tbl", "data.frame"),
    vars = list(sym("x"))
  )
  g <- expect_warning(group_data(df))
  expect_equal(g$x, 1)
  expect_equal(g$.rows, list(1L))
  expect_equal(attr(g, ".drop"), TRUE)
  expect_null(attr(df, "vars"))
})

test_that("old group format repair does not keep a vars attribute around", {
  tbl <- tibble(x = 1:10, y = 1:10)
  attr(tbl, "vars") <- rlang::sym("x")
  class(tbl) <- c("grouped_df", "tbl_df", "tbl", "data.frame")

  expect_warning({
    res <- tbl %>% group_by(y)
  })
  expect_equal(group_vars(res), "y")
  expect_null(attr(res, " vars"))
  expect_null(attr(tbl, " vars"))
})
