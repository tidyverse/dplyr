test_that("select preserves grouping", {
  gf <- group_by(tibble(g = 1:3, x = 3:1), g)

  i <- count_regroups(out <- select(gf, h = g))
  expect_equal(i, 0)
  expect_equal(group_vars(out), "h")
})

test_that("grouping variables preserved with a message (#1511)", {
  df <- tibble(g = 1:3, x = 3:1) %>% group_by(g)

  expect_message(res <- select(df, x), "Adding missing grouping variables")
  expect_named(res, c("g", "x"))
})

test_that("non-syntactic grouping variable is preserved (#1138)", {
  df <- tibble(`a b` = 1L) %>% group_by(`a b`) %>% select()
  expect_named(df, "a b")
})

test_that("select doesn't fail if some names missing", {
  df1 <- data.frame(x = 1:10, y = 1:10, z = 1:10)
  df2 <- setNames(df1, c("x", "y", ""))
  # df3 <- setNames(df1, c("x", "", ""))

  expect_equal(select(df1, x), data.frame(x = 1:10))
  expect_equal(select(df2, x), data.frame(x = 1:10))
  # expect_equal(select(df3, x), data.frame(x = 1:10))
})


# Special cases -------------------------------------------------

test_that("select with no args returns nothing", {
  empty <- select(mtcars)
  expect_equal(ncol(empty), 0)
  expect_equal(nrow(empty), 32)

  empty <- select(mtcars, !!!list())
  expect_equal(ncol(empty), 0)
  expect_equal(nrow(empty), 32)
})

test_that("select excluding all vars returns nothing", {
  expect_equal(dim(select(mtcars, -(mpg:carb))), c(32, 0))
  expect_equal(dim(select(mtcars, starts_with("x"))), c(32, 0))
  expect_equal(dim(select(mtcars, -matches("."))), c(32, 0))
})

test_that("negating empty match returns everything", {
  df <- data.frame(x = 1:3, y = 3:1)
  expect_equal(select(df, -starts_with("xyz")), df)
})

test_that("can select with duplicate columns", {
  df <- tibble(x = 1, x = 2, y = 1, .name_repair = "minimal")

  # can extract duplicate cols by position
  expect_named(df %>% select(1, 3), c("x", "y"))

  # can select out non-duplicated columns
  expect_named(df %>% select(y), "y")
})

# Select variables -----------------------------------------------

test_that("select can be before group_by (#309)", {
  df <- data.frame(
    id = c(1, 1, 2, 2, 2, 3, 3, 4, 4, 5),
    year = c(2013, 2013, 2012, 2013, 2013, 2013, 2012, 2012, 2013, 2013),
    var1 = rnorm(10)
  )
  dfagg <- df %>%
    group_by(id, year) %>%
    select(id, year, var1) %>%
    summarise(var1 = mean(var1))
  expect_equal(names(dfagg), c("id", "year", "var1"))
})


test_that("select succeeds in presence of raw columns (#1803)", {
  df <- tibble(a = 1:3, b = as.raw(1:3))
  expect_identical(select(df, a), df["a"])
  expect_identical(select(df, b), df["b"])
  expect_identical(select(df, -b), df["a"])
})

test_that("arguments to select() don't match vars_select() arguments", {
  df <- tibble(a = 1)
  expect_identical(select(df, var = a), tibble(var = 1))
  expect_identical(select(group_by(df, a), var = a), group_by(tibble(var = 1), var))
  expect_identical(select(df, exclude = a), tibble(exclude = 1))
  expect_identical(select(df, include = a), tibble(include = 1))
  expect_identical(select(group_by(df, a), exclude = a), group_by(tibble(exclude = 1), exclude))
  expect_identical(select(group_by(df, a), include = a), group_by(tibble(include = 1), include))
})

test_that("can select() with .data pronoun (#2715)", {
  expect_identical(select(mtcars, .data$cyl), select(mtcars, cyl))
})

test_that("can select() with character vectors", {
  expect_identical(select(mtcars, "cyl", !!"disp", c("cyl", "am", "drat")), mtcars[c("cyl", "disp", "am", "drat")])
})

test_that("select() treats NULL inputs as empty", {
  expect_identical(select(mtcars, cyl), select(mtcars, NULL, cyl, NULL))
})

test_that("can select() with strings and character vectors", {
  vars <- c(foo = "cyl", bar = "am")

  expect_identical(select(mtcars, !!!vars), select(mtcars, foo = cyl, bar = am))
  expect_identical(select(mtcars, !!vars), select(mtcars, foo = cyl, bar = am))
})

test_that("select works on empty names (#3601)", {
  df <- data.frame(x=1, y=2, z=3)
  colnames(df) <- c("x","y","")
  expect_identical(select(df, x)$x, 1)

  colnames(df) <- c("","y","z")
  expect_identical(select(df, y)$y, 2)
})

test_that("select works on NA names (#3601)", {
  df <- data.frame(x=1, y=2, z=3)
  colnames(df) <- c("x","y",NA)
  expect_identical(select(df, x)$x, 1)

  colnames(df) <- c(NA,"y","z")
  expect_identical(select(df, y)$y, 2)
})


# dplyr_col_select() ------------------------------------------------------

test_that("dplyr_col_select() aborts when `[` implementation is broken", {
  local_methods(
    "[.dplyr_test_broken_operator" = function(x, ...) {
      unclass(x)
    },
    "[.dplyr_test_operator_wrong_size" = function(x, ...) {
      data.frame()
    }
  )
  df1 <- new_tibble(list(x = 1), nrow = 1L, class = "dplyr_test_broken_operator")
  expect_error(dplyr_col_select(df1, 1:2))
  expect_error(dplyr_col_select(df1, 0))

  df2 <- new_tibble(list(x = 1), nrow = 1L, class = "dplyr_test_operator_wrong_size")
  expect_error(dplyr_col_select(d2f, 1:2))

  verify_output(test_path("test-select-errors.txt"), {
    "# from vctrs"
    dplyr_col_select(df1, 2)

    "# not returning a data frame"
    dplyr_col_select(df1, 1)

    "# unexpected number of columns"
    dplyr_col_select(df2, 1)
  })
})
