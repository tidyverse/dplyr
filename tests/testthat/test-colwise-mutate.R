context("colwise mutate/summarise")

test_that("can use character vectors or bare functions", {
  df <- data.frame(x = 1:3)
  expect_equal(summarise_all(df, "mean"), data.frame(x = 2))
  expect_equal(summarise_all(df, mean), data.frame(x = 2))

  expect_equal(mutate_all(df, list(x = "mean")), data.frame(x = rep(2, 3)))
  expect_equal(mutate_all(df, list(x = mean)), data.frame(x = rep(2, 3)))
})

test_that("default names are smallest unique set", {
  df <- data.frame(x = 1:3, y = 1:3)

  expect_named(summarise_at(df, vars(x:y), list(mean)), c("x", "y"))
  expect_named(summarise_at(df, vars(x), list(mean = mean, sd = sd)), c("mean", "sd"))
  expect_named(summarise_at(df, vars(x:y), list(mean = mean, sd = sd)), c("x_mean", "y_mean", "x_sd", "y_sd"))
})

test_that("named arguments force complete names", {
  df <- data.frame(x = 1:3, y = 1:3)

  expect_named(summarise_at(df, vars(x:y), list(mean = mean)), c("x_mean", "y_mean"))
  expect_named(summarise_at(df, vars(x = x), list(mean = mean, sd = sd)), c("x_mean", "x_sd"))
})

expect_classes <- function(tbl, expected) {
  classes <- unname(map_chr(tbl, class))
  classes <- paste0(substring(classes, 0, 1), collapse = "")
  expect_equal(classes, expected)
}

test_that("can select colwise", {
  columns <- iris %>% mutate_at(NULL, as.character)
  expect_classes(columns, "nnnnf")

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
  df <- tibble(`x 1` = 1:3)
  expect_identical(summarise_at(df, "x 1", sum)[[1]], 6L)
  expect_identical(summarise_if(df, is.numeric, sum)[[1]], 6L)
  expect_identical(summarise_all(df, sum)[[1]], 6L)
  expect_identical(mutate_all(df, `*`, 2)[[1]], (1:3) * 2)
})

test_that("empty selection does not select everything (#2009, #1989)", {
  expect_equal(
    tibble::remove_rownames(mtcars),
    tibble::remove_rownames(mutate_if(mtcars, is.factor, as.character))
  )
})

test_that("error is thrown with improper additional arguments", {
  # error messages by base R, not checked
  expect_error(mutate_all(mtcars, round, 0, 0))
  expect_error(mutate_all(mtcars, mean, na.rm = TRUE, na.rm = TRUE))
})

test_that("predicate can be quoted", {
  expected <- mutate_if(mtcars, is_integerish, mean)
  expect_identical(mutate_if(mtcars, "is_integerish", mean), expected)
  expect_identical(mutate_if(mtcars, ~ is_integerish(.x), mean), expected)
})

test_that("transmute verbs do not retain original variables", {
  expect_named(transmute_all(tibble(x = 1:3, y = 1:3), list(mean = mean, sd = sd)), c("x_mean", "y_mean", "x_sd", "y_sd"))
  expect_named(transmute_if(tibble(x = 1:3, y = 1:3), is_integer, list(mean = mean, sd = sd)), c("x_mean", "y_mean", "x_sd", "y_sd"))
  expect_named(transmute_at(tibble(x = 1:3, y = 1:3), vars(x:y), list(mean = mean, sd = sd)), c("x_mean", "y_mean", "x_sd", "y_sd"))
})

test_that("can rename with vars() (#2594)", {
  expect_identical(
    mutate_at(tibble(x = 1:3), vars(y = x), mean),
    tibble(x = 1:3, y = c(2, 2, 2))
  )
})

test_that("selection works with grouped data frames (#2624)", {
  gdf <- group_by(iris, Species)
  expect_identical(mutate_if(gdf, is.factor, as.character), gdf)
})

test_that("at selection works even if not all ops are named (#2634)", {
  df <- tibble(x = 1, y = 2)
  expect_identical(mutate_at(df, vars(z = x, y), list(~. + 1)), tibble(x = 1, y = 3, z = 2))
})

test_that("can use a purrr-style lambda", {
  expect_identical(summarise_at(mtcars, vars(1:2), ~ mean(.x)), summarise(mtcars, mpg = mean(mpg), cyl = mean(cyl)))
})

test_that("mutate_at and transmute_at refuses to mutate a grouping variable (#3351, #3480)", {
  tbl <- tibble(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8) %>%
    group_by(gr1)

  expect_error(
    mutate_at(tbl, vars(gr1), sqrt),
    "Column `gr1` can't be modified because it's a grouping variable",
    fixed = TRUE
  )

  expect_error(
    transmute_at(tbl, vars(gr1), sqrt),
    "Column `gr1` can't be modified because it's a grouping variable",
    fixed = TRUE
  )
})

test_that("mutate and transmute variants does not mutate grouping variable (#3351, #3480)", {
  tbl <- tibble(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8) %>%
    group_by(gr1)
  res <- mutate(tbl, gr2 = sqrt(gr2), x = sqrt(x))

  expect_message(expect_identical(mutate_all(tbl, sqrt), res), "ignored")
  expect_message(expect_identical(transmute_all(tbl, sqrt), res), "ignored")

  expect_message(expect_identical(mutate_if(tbl, is.integer, sqrt), res), "ignored")
  expect_message(expect_identical(transmute_if(tbl, is.integer, sqrt), res), "ignored")

  expect_identical(transmute_at(tbl, vars(-group_cols()), sqrt), res)
  expect_identical(mutate_at(tbl, vars(-group_cols()), sqrt), res)
})

test_that("summarise_at refuses to treat grouping variables (#3351, #3480)", {
  tbl <- tibble(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8) %>%
    group_by(gr1)

  expect_error(
    summarise_at(tbl, vars(gr1), mean)
  )
})

test_that("summarise variants does not summarise grouping variable (#3351, #3480)", {
  tbl <- tibble(gr1 = rep(1:2, 4), gr2 = rep(1:2, each = 4), x = 1:8) %>%
    group_by(gr1)
  res <- summarise(tbl, gr2 = mean(gr2), x = mean(x))

  expect_identical(summarise_all(tbl, mean), res)
  expect_identical(summarise_if(tbl, is.integer, mean), res)
})

test_that("summarise_at removes grouping variables (#3613)", {
  d <- tibble( x = 1:2, y = 3:4, g = 1:2) %>% group_by(g)
  res <- d %>%
    group_by(g) %>%
    summarise_at(-1, mean)

  expect_equal(names(res), c("g", "y"))
})

test_that("group_by_(at,all) handle utf-8 names (#3829)", {
  with_non_utf8_encoding({
    name <- get_native_lang_string()
    tbl <- tibble(a = 1) %>%
      setNames(name)

    res <- group_by_all(tbl) %>% groups()
    expect_equal(res[[1]], sym(name))

    res <- group_by_at(tbl, name) %>% groups()
    expect_equal(res[[1]], sym(name))
  })
})

test_that("*_(all,at) handle utf-8 names (#2967)", {
  with_non_utf8_encoding({
    name <- get_native_lang_string()
    tbl <- tibble(a = 1) %>%
      setNames(name)

    res <- tbl %>%
      mutate_all(list(as.character)) %>%
      names()
    expect_equal(res, name)

    res <- tbl %>%
      mutate_at(name, list(as.character)) %>%
      names()
    expect_equal(res, name)

    res <- tbl %>%
      summarise_all(list(as.character)) %>%
      names()
    expect_equal(res, name)

    res <- tbl %>%
      summarise_at(name, list(as.character)) %>%
      names()
    expect_equal(res, name)

    res <- select_at(tbl, name) %>% names()
    expect_equal(res, name)
  })
})

test_that("summarise_at with multiple columns AND unnamed functions works (#4119)", {
  res <- storms %>%
    summarise_at(vars(wind, pressure), list(mean, median))

  expect_equal(ncol(res), 4L)
  expect_equal(names(res), c("wind_fn1", "pressure_fn1", "wind_fn2", "pressure_fn2"))

  res <- storms %>%
    summarise_at(vars(wind, pressure), list(n = length, mean, median))

  expect_equal(ncol(res), 6L)
  expect_equal(names(res), c("wind_n", "pressure_n", "wind_fn1", "pressure_fn1", "wind_fn2", "pressure_fn2"))
})

test_that("mutate_at with multiple columns AND unnamed functions works (#4119)", {
  res <- storms %>%
    mutate_at(vars(wind, pressure), list(mean, median))

  expect_equal(ncol(res), ncol(storms) + 4L)
  expect_equal(
    names(res),
    c(names(storms), c("wind_fn1", "pressure_fn1", "wind_fn2", "pressure_fn2"))
  )
})

test_that("colwise mutate have .data in scope of rlang lambdas (#4183)", {
  results <- list(
    iris %>% mutate_if(is.numeric, ~ . / iris$Petal.Width),
    iris %>% mutate_if(is.numeric, ~ . / Petal.Width),
    iris %>% mutate_if(is.numeric, ~ . / .data$Petal.Width),

    iris %>% mutate_if(is.numeric, list(~ . / iris$Petal.Width )),
    iris %>% mutate_if(is.numeric, list(~ . / Petal.Width      )),
    iris %>% mutate_if(is.numeric, list(~ . / .data$Petal.Width)),

    iris %>% mutate_if(is.numeric, ~ .x / iris$Petal.Width),
    iris %>% mutate_if(is.numeric, ~ .x / Petal.Width),
    iris %>% mutate_if(is.numeric, ~ .x / .data$Petal.Width),

    iris %>% mutate_if(is.numeric, list(~ .x / iris$Petal.Width )),
    iris %>% mutate_if(is.numeric, list(~ .x / Petal.Width      )),
    iris %>% mutate_if(is.numeric, list(~ .x / .data$Petal.Width))
  )

  for(i in 2:12) {
    expect_equal(results[[1]], results[[i]])
  }

})

test_that("can choose the name of vars with multiple funs (#4180)", {
  expect_identical(
    mtcars %>%
      group_by(cyl) %>%
      summarise_at(vars(DISP = disp), list(mean = mean, median = median)),
    mtcars %>%
      group_by(cyl) %>%
      summarise(DISP_mean = mean(disp), DISP_median = median(disp))
  )
})

test_that("summarise_at() unquotes in lambda (#4287)", {
  df <- tibble::tibble(year = seq(2015, 2050, 5), P = 5.0 + 2.5 * year)
  year <- 2037

  expect_equal(
    summarise_at(df, vars(-year), ~approx(x = year, y = ., xout = !!year)$y),
    summarise(df, P = approx(x = year, y = P, xout = !!year)$y)
  )
})

test_that("mutate_at() unquotes in lambdas (#4199)", {
  df <- tibble(a = 1:10, b = runif(1:10), c = letters[1:10])
  varname <- "a"
  symname <- rlang::sym(varname)
  quoname <- enquo(symname)

  expect_identical(
    df %>% mutate(b = mean(!!quoname)),
    df %>% mutate_at(vars(matches("b")), list(~mean(!!quoname)))
  )
})

test_that("summarise_at() can refer to local variables and columns (#4304)", {

  # using local here in case someone wants to run the content of the test
  # as opposed to the test_that() call
  res <- local({
    value <- 10
    expect_identical(
      iris %>% summarise_at("Sepal.Length", ~ sum(. / value)),
      iris %>% summarise(Sepal.Length = sum(Sepal.Length / value))
    )
  })

})

test_that("colwise mutate handles formulas with constants (#4374)", {
  expect_identical(
    tibble(x = 12) %>% mutate_all(~ 42),
    tibble(x = 42)
  )
  expect_identical(
    tibble(x = 12) %>% mutate_at("x", ~ 42),
    tibble(x = 42)
  )
})

test_that("colwise mutate gives correct error message if column not found (#4374)", {
  expect_error(
    mutate_at(tibble(), "test", ~ 1)
  )
})

test_that("colwise mutate handle named chr vectors", {
  res <- tibble(x = 1:10) %>%
    mutate_at(c(y = "x"), mean)
  expect_identical(res, tibble(x = 1:10, y = 5.5))
})

test_that("colwise verbs soft deprecate quosures (#4330)", {
  with_lifecycle_errors({
    expect_error(
      mutate_at(mtcars, vars(mpg), quo(mean(.)))
    )
    expect_error(
      summarise_at(mtcars, vars(mpg), quo(mean(.)))
    )
  })

  expect_equal(
    transmute_at(mtcars, vars(mpg), ~. > mean(.)),
    transmute_at(mtcars, vars(mpg), quo(. > mean(.)))
  )
})
