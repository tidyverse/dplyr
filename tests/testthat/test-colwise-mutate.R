context("colwise mutate/summarise")

test_that("funs found in current environment", {
  f <- function(x) 1
  df <- data.frame(x = c(2:10, 1000))

  out <- summarise_all(df, funs(f, mean, median))
  expect_equal(out, data.frame(f = 1, mean = 105.4, median = 6.5))
})

test_that("can use character vectors", {
  df <- data.frame(x = 1:3)

  expect_equal(summarise_all(df, "mean"), summarise_all(df, funs(mean)))
  expect_equal(mutate_all(df, list(mean = "mean")), mutate_all(df, funs(mean = mean)))
})

test_that("can use bare functions", {
  df <- data.frame(x = 1:3)

  expect_equal(summarise_all(df, mean), summarise_all(df, funs(mean)))
  expect_equal(mutate_all(df, mean), mutate_all(df, funs(mean)))
})

test_that("default names are smallest unique set", {
  df <- data.frame(x = 1:3, y = 1:3)

  expect_named(summarise_at(df, vars(x:y), funs(mean)), c("x", "y"))
  expect_named(summarise_at(df, vars(x), funs(mean, sd)), c("mean", "sd"))
  expect_named(summarise_at(df, vars(x:y), funs(mean, sd)), c("x_mean", "y_mean", "x_sd", "y_sd"))
  expect_named(summarise_at(df, vars(x:y), funs(base::mean, stats::sd)), c("x_base::mean", "y_base::mean", "x_stats::sd", "y_stats::sd"))
})

test_that("named arguments force complete named", {
  df <- data.frame(x = 1:3, y = 1:3)
  expect_named(summarise_at(df, vars(x:y), funs(mean = mean)), c("x_mean", "y_mean"))
  expect_named(summarise_at(df, vars(x = x), funs(mean, sd)), c("x_mean", "x_sd"))
})

expect_classes <- function(tbl, expected) {
  classes <- unname(map_chr(tbl, class))
  classes <- paste0(substring(classes, 0, 1), collapse = "")
  expect_equal(classes, expected)
}

test_that("can select colwise", {
  columns <- iris %>% mutate_at(vars(starts_with("Petal")), as.character)
  expect_classes(columns, "nnccf")

  numeric <- iris %>% mutate_at(c(1, 3), as.character)
  expect_classes(numeric, "cncnf")

  character <- iris %>% mutate_at("Species", as.character)
  expect_classes(character, "nnnnc")
})

test_that("can probe colwise", {
  predicate <- iris %>% mutate_if(is.factor, as.character)
  expect_classes(predicate, "nnnnc")

  logical <- iris %>% mutate_if(c(TRUE, FALSE, TRUE, TRUE, FALSE), as.character)
  expect_classes(logical, "cnccf")
})

test_that("non syntactic colnames work", {
  df <- data_frame(`x 1` = 1:3)
  expect_identical(summarise_at(df, "x 1", sum)[[1]], 6L)
  expect_identical(summarise_if(df, is.numeric, sum)[[1]], 6L)
  expect_identical(summarise_all(df, sum)[[1]], 6L)
  expect_identical(mutate_all(df, `*`, 2)[[1]], (1:3) * 2)
})

test_that("empty selection does not select everything (#2009, #1989)", {
  expect_equal(mtcars, mutate_if(mtcars, is.factor, as.character))
})

test_that("error is thrown with improper additional arguments", {
  expect_error(mutate_all(mtcars, round, 0, 0), "3 arguments passed")
  expect_error(mutate_all(mtcars, mean, na.rm = TRUE, na.rm = TRUE), "matched by multiple")
})

test_that("predicate can be quoted", {
  expected <- mutate_if(mtcars, is_integerish, mean)
  expect_identical(mutate_if(mtcars, "is_integerish", mean), expected)
  expect_identical(mutate_if(mtcars, ~is_integerish(.x), mean), expected)
})

test_that("transmute verbs do not retain original variables", {
  expect_named(transmute_all(data_frame(x = 1:3, y = 1:3), funs(mean, sd)), c("x_mean", "y_mean", "x_sd", "y_sd"))
  expect_named(transmute_if(data_frame(x = 1:3, y = 1:3), is_integer, funs(mean, sd)), c("x_mean", "y_mean", "x_sd", "y_sd"))
  expect_named(transmute_at(data_frame(x = 1:3, y = 1:3), vars(x:y), funs(mean, sd)), c("x_mean", "y_mean", "x_sd", "y_sd"))
})

test_that("can rename with vars() (#2594)", {
  expect_equal(mutate_at(tibble(x = 1:3), vars(y = x), mean), tibble(x = 1:3, y = c(2, 2, 2)))
})

test_that("selection works with grouped data frames (#2624)", {
  gdf <- group_by(iris, Species)
  expect_identical(mutate_if(gdf, is.factor, as.character), gdf)
})


# Deprecated ---------------------------------------------------------

test_that("_each() and _all() families agree", {
  df <- data.frame(x = 1:3, y = 1:3)

  expect_warning(expect_equal(summarise_each(df, funs(mean)), summarise_all(df, mean)), "deprecated")
  expect_warning(expect_equal(summarise_each(df, funs(mean), x:y), summarise_at(df, vars(x:y), mean)), "deprecated")
  expect_warning(expect_equal(summarise_each(df, funs(mean), z = y), summarise_at(df, vars(z = y), mean)), "deprecated")

  expect_warning(expect_equal(mutate_each(df, funs(mean)), mutate_all(df, mean)), "deprecated")
  expect_warning(expect_equal(mutate_each(df, funs(mean), x:y), mutate_at(df, vars(x:y), mean)), "deprecated")
  expect_warning(expect_equal(mutate_each(df, funs(mean), z = y), mutate_at(df, vars(z = y), mean)), "deprecated")
})
