context("Select")

test_that("select does not lose grouping (#147)", {
  df <- tibble(a = rep(1:4, 2), b = rep(1:4, each = 2), x = runif(8))
  grouped <- df %>% group_by(a) %>% select(a, b, x)

  expect_groups(grouped, "a")
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


# Empty selects -------------------------------------------------

test_that("select with no args returns nothing", {
  empty <- select(mtcars)
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

test_that("rename errors with invalid grouped data frame (#640)", {
  df <- tibble(a = 1:3, b = 2:4, d = 3:5) %>% group_by(a, b)
  df$a <- NULL
  expect_error(
    df %>% rename(e = d),
    "not found in groups metadata"
  )
  expect_error(
    df %>% rename(e = b),
    "not found in groups metadata"
  )
})

test_that("rename() handles data pronoun", {
  expect_identical(rename(tibble(x = 1), y = .data$x), tibble(y = 1))
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

test_that("arguments to rename() don't match vars_rename() arguments (#2861)", {
  df <- tibble(a = 1)
  expect_identical(rename(df, var = a), tibble(var = 1))
  expect_identical(rename(group_by(df, a), var = a), group_by(tibble(var = 1), var))
  expect_identical(rename(df, strict = a), tibble(strict = 1))
  expect_identical(rename(group_by(df, a), strict = a), group_by(tibble(strict = 1), strict))
})

test_that("can select() with .data pronoun (#2715)", {
  expect_identical(select(mtcars, .data$cyl), select(mtcars, cyl))
})

test_that("can select() with character vectors", {
  expect_identical(select(mtcars, "cyl", !!"disp", c("cyl", "am", "drat")), mtcars[c("cyl", "disp", "am", "drat")])
})

test_that("rename() to UTF-8 column names", {
  skip_on_os("windows") # needs an rlang update? #3049
  df <- tibble(a = 1) %>% rename("\u5e78" := a)

  expect_equal(colnames(df), "\u5e78")
})

test_that("select() treats NULL inputs as empty", {
  expect_identical(select(mtcars, cyl), select(mtcars, NULL, cyl, NULL))
})

test_that("can select() or rename() with strings and character vectors", {
  vars <- c(foo = "cyl", bar = "am")

  expect_identical(select(mtcars, !!!vars), select(mtcars, foo = cyl, bar = am))
  expect_identical(select(mtcars, !!vars), select(mtcars, foo = cyl, bar = am))

  expect_identical(rename(mtcars, !!!vars), rename(mtcars, foo = cyl, bar = am))
  expect_identical(rename(mtcars, !!vars), rename(mtcars, foo = cyl, bar = am))
})

test_that("select works on empty names (#3601)", {
  df <- data.frame(x=1, y=2, z=3)
  colnames(df) <- c("x","y","")
  expect_identical(select(df, x)$x, 1)

  colnames(df) <- c("","y","z")
  expect_identical(select(df, y)$y, 2)
})

test_that("select works on NA names (#3601)", {
  skip("to be discussed")
  df <- data.frame(x=1, y=2, z=3)
  colnames(df) <- c("x","y",NA)
  expect_identical(select(df, x)$x, 1)

  colnames(df) <- c(NA,"y","z")
  expect_identical(select(df, y)$y, 2)
})
