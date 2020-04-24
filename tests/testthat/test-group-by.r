context("Group by")

df <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))

test_that("group_by with .add = TRUE adds groups", {
  add_groups1 <- function(tbl) group_by(tbl, x, y, .add = TRUE)
  add_groups2 <- function(tbl) group_by(group_by(tbl, x, .add = TRUE), y, .add = TRUE)

  expect_equal(group_vars(add_groups1(df)), c("x", "y"))
  expect_equal(group_vars(add_groups2(df)), c("x", "y"))
})

test_that("add = TRUE is deprecated", {
  rlang::scoped_options(lifecycle_verbosity = "warning")

  df <- tibble(x = 1, y = 2)

  expect_warning(
    out <- df %>% group_by(x) %>% group_by(y, add = TRUE),
    "deprecated"
  )
  expect_equal(group_vars(out), c("x", "y"))
})

test_that("joins preserve grouping", {
  g <- group_by(df, x)

  expect_equal(group_vars(inner_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(left_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(semi_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(anti_join(g, g, by = c("x", "y"))), "x")
})

test_that("constructors drops groups", {
  df <- data.frame(x = 1:3) %>% group_by(x)
  expect_equal(group_vars(as_tibble(df)), character())
})

test_that("grouping by constant adds column (#410)", {
  grouped <- group_by(mtcars, "cyl") %>% summarise(foo = n())
  expect_equal(names(grouped), c('"cyl"', "foo"))
  expect_equal(nrow(grouped), 1L)
})

test_that(".dots is soft deprecated", {
  rlang::scoped_options(lifecycle_verbosity = "warning")

  df <- tibble(x = 1, y = 1)
  expect_warning(gf <- group_by(df, .dots = "x"), "deprecated")
})

# Test full range of variable types --------------------------------------------


test_that("local group_by preserves variable types", {
  df_var <- tibble(
    l = c(T, F),
    i = 1:2,
    d = Sys.Date() + 1:2,
    f = factor(letters[1:2]),
    num = 1:2 + 0.5,
    t = Sys.time() + 1:2,
    c = letters[1:2]
  )
  attr(df_var$t, "tzone") <- ""

  for (var in names(df_var)) {
    expected <- tibble(!!var := sort(unique(df_var[[var]])), n = 1L)

    summarised <- df_var %>% group_by(!!sym(var)) %>% summarise(n = n())
    expect_equal(summarised, expected)
  }
})

test_that("mutate does not loose variables (#144)", {
  df <- tibble(a = rep(1:4, 2), b = rep(1:4, each = 2), x = runif(8))
  by_ab <- group_by(df, a, b)
  by_a <- summarise(by_ab, x = sum(x))
  by_a_quartile <- group_by(by_a, quartile = ntile(x, 4))

  expect_equal(names(by_a_quartile), c("a", "b", "x", "quartile"))
})

test_that("group_by uses shallow copy", {
  m1 <- group_by(mtcars, cyl)
  expect_equal(group_vars(mtcars), character())

  expect_equal(
    lobstr::obj_addrs(mtcars),
    lobstr::obj_addrs(m1)
  )
})

test_that("group_by orders by groups. #242", {
  df <- data.frame(a = sample(1:10, 3000, replace = TRUE)) %>% group_by(a)
  expect_equal(group_data(df)$a, 1:10)

  df <- data.frame(a = sample(letters[1:10], 3000, replace = TRUE), stringsAsFactors = FALSE) %>% group_by(a)
  expect_equal(group_data(df)$a, letters[1:10])

  df <- data.frame(a = sample(sqrt(1:10), 3000, replace = TRUE)) %>% group_by(a)
  expect_equal(group_data(df)$a, sqrt(1:10))
})

test_that("Can group_by() a POSIXlt", {
  skip_if_not_installed("tibble", "2.99.99")
  df <- tibble(x = 1:5, times = as.POSIXlt(seq.Date(Sys.Date(), length.out = 5, by = "day")))
  g <- group_by(df, times)
  expect_equal(nrow(group_data(g)), 5L)
})

test_that("group_by() handles list as grouping variables", {
  df <- tibble(x = 1:3, y = list(1:2, 1:3, 1:2))
  gdata <- group_data(group_by(df, y))
  expect_equal(nrow(gdata), 2L)
  expect_equal(gdata$y, list(1:2, 1:3))
  expect_equal(gdata$.rows, list_of(c(1L, 3L), 2L))
})

test_that("select(group_by(.)) implicitely adds grouping variables (#170)", {
  res <- mtcars %>% group_by(vs) %>% select(mpg)
  expect_equal(names(res), c("vs", "mpg"))
})

test_that("group_by only creates one group for NA (#401)", {
  x <- as.numeric(c(NA, NA, NA, 10:1, 10:1))
  w <- c(20, 30, 40, 1:10, 1:10) * 10

  n_distinct(x) # 11 OK
  res <- data.frame(x = x, w = w) %>% group_by(x) %>% summarise(n = n())
  expect_equal(nrow(res), 11L)
})

test_that("there can be 0 groups (#486)", {
  data <- tibble(a = numeric(0), g = character(0)) %>% group_by(g)
  expect_equal(length(data$a), 0L)
  expect_equal(length(data$g), 0L)
  expect_equal(map_int(group_rows(data), length), integer(0))
})

test_that("group_by works with zero-row data frames (#486)", {
  df <- data.frame(a = numeric(0), b = numeric(0), g = character(0))
  dfg <- group_by(df, g, .drop = FALSE)
  expect_equal(dim(dfg), c(0, 3))
  expect_equal(group_vars(dfg), "g")
  expect_equal(group_size(dfg), integer(0))

  x <- summarise(dfg, n = n())
  expect_equal(dim(x), c(0, 2))
  expect_equal(group_vars(x), character())

  x <- mutate(dfg, c = b + 1)
  expect_equal(dim(x), c(0, 4))
  expect_equal(group_vars(x), "g")
  expect_equal(group_size(x), integer(0))

  x <- filter(dfg, a == 100)
  expect_equal(dim(x), c(0, 3))
  expect_equal(group_vars(x), "g")
  expect_equal(group_size(x), integer(0))

  x <- arrange(dfg, a, g)
  expect_equal(dim(x), c(0, 3))
  expect_equal(group_vars(x), "g")
  expect_equal(group_size(x), integer(0))

  x <- select(dfg, a) # Only select 'a' column; should result in 'g' and 'a'
  expect_equal(dim(x), c(0, 2))
  expect_equal(group_vars(x), "g")
  expect_equal(group_size(x), integer(0))
})

test_that("[ on grouped_df preserves grouping if subset includes grouping vars", {
  df <- tibble(x = 1:5, ` ` = 6:10)
  by_x <- df %>% group_by(x)
  expect_equal(by_x %>% groups(), by_x %>% `[`(1:2) %>% groups())

  # non-syntactic name
  by_ns <- df %>% group_by(` `)
  expect_equal(by_ns %>% groups(), by_ns %>% `[`(1:2) %>% groups())
})

test_that("[ on grouped_df drops grouping if subset doesn't include grouping vars", {
  by_cyl <- mtcars %>% group_by(cyl)
  no_cyl <- by_cyl %>% `[`(c(1, 3))

  expect_equal(group_vars(no_cyl), character())
  expect_is(no_cyl, "tbl_df")
})

test_that("group_by works after arrange (#959)", {
  df <- tibble(Log = c(1, 2, 1, 2, 1, 2), Time = c(10, 1, 3, 0, 15, 11))
  res <- df %>%
    arrange(Time) %>%
    group_by(Log) %>%
    mutate(Diff = Time - lag(Time))
  expect_true(all(is.na(res$Diff[c(1, 3)])))
  expect_equal(res$Diff[c(2, 4, 5, 6)], c(1, 7, 10, 5))
})

test_that("group_by keeps attributes", {
  d <- data.frame(x = structure(1:10, foo = "bar"))
  gd <- group_by(d)
  expect_equal(attr(gd$x, "foo"), "bar")
})

test_that("ungroup.rowwise_df gives a tbl_df (#936)", {
  res <- mtcars %>% rowwise() %>% ungroup() %>% class()
  expect_equal(res, c("tbl_df", "tbl", "data.frame"))
})

test_that(paste0("group_by handles encodings for native strings (#1507)"), {
  with_non_utf8_encoding({
    special <- get_native_lang_string()

    df <- data.frame(x = 1:3, Eng = 2:4)

    for (names_converter in c(enc2native, enc2utf8)) {
      for (dots_converter in c(enc2native, enc2utf8)) {
        names(df) <- names_converter(c(special, "Eng"))
        res <- group_by(df, !!!syms(dots_converter(special)))
        expect_equal(names(res), names(df))
        expect_equal(group_vars(res), special)
      }
    }

    for (names_converter in c(enc2native, enc2utf8)) {
      names(df) <- names_converter(c(special, "Eng"))

      res <- group_by(df, !!!special)
      expect_equal(names(res), c(names(df), deparse(special)))
      expect_equal(groups(res), list(as.name(enc2native(deparse(special)))))
    }
  })
})

test_that("group_by handles raw columns (#1803)", {
  df <- tibble(a = 1:3, b = as.raw(1:3))
  expect_identical(ungroup(group_by(df, a)), df)
  expect_identical(ungroup(group_by(df, b)), df)
})

test_that("rowwise handles raw columns (#1803)", {
  df <- tibble(a = 1:3, b = as.raw(1:3))
  expect_is(rowwise(df), "rowwise_df")
})

test_that("group_by() names pronouns correctly (#2686)", {
  expect_named(group_by(tibble(x = 1), .data$x), "x")
  expect_named(group_by(tibble(x = 1), .data[["x"]]), "x")
})

test_that("group_by() does not affect input data (#3028)", {
  x <-
    data.frame(old1 = c(1, 2, 3), old2 = c(4, 5, 6)) %>%
    group_by(old1)

  y <-
    x %>%
    select(new1 = old1, new2 = old2)

  expect_identical(groups(x), syms(quote(old1)))
})

test_that("group_by() does not mutate for nothing when using the .data pronoun (#2752, #3533)", {
  expect_identical(
    iris %>% group_by(Species) %>% group_by(.data$Species),
    iris %>% group_by(Species)
  )
  expect_identical(
    iris %>% group_by(Species) %>% group_by(.data[["Species"]]),
    iris %>% group_by(Species)
  )

  df <- tibble(x = 1:5)
  attr(df, "y") <- 1

  expect_equal( df %>% group_by(.data$x) %>% attr("y"), 1 )
  expect_equal( df %>% group_by(.data[["x"]]) %>% attr("y"), 1 )
})

test_that("tbl_sum gets the right number of groups", {
  res <- data.frame(x=c(1,1,2,2)) %>% group_by(x) %>% tbl_sum()
  expect_equal(res, c("A tibble" = "4 x 1", "Groups" = "x [2]"))
})

test_that("group_by ignores empty quosures (3780)", {
  empty <- quo()
  expect_equal(group_by(mtcars, cyl), group_by(mtcars, cyl, !!empty))
})

# Zero groups ---------------------------------------------------

test_that("mutate handles grouped tibble with 0 groups (#3935)", {
  df <- tibble(x=integer()) %>% group_by(x)
  res <- mutate(df, y = mean(x), z = +mean(x), n = n())
  expect_equal(names(res), c("x", "y", "z", "n"))
  expect_equal(nrow(res), 0L)
  expect_equal(res$y, double())
  expect_equal(res$z, double())
  expect_equal(res$n, integer())
})

test_that("summarise handles grouped tibble with 0 groups (#3935)", {
  df <- tibble(x=integer()) %>% group_by(x)
  res <- summarise(df, y = mean(x), z = +mean(x), n = n())
  expect_equal(names(res), c("x", "y", "z", "n"))
  expect_equal(nrow(res), 0L)
  expect_equal(res$y, double())
  expect_equal(res$n, integer())
  expect_equal(res$z, double())
})

test_that("filter handles grouped tibble with 0 groups (#3935)", {
  df <- tibble(x=integer()) %>% group_by(x)
  res <- filter(df, x > 3L)
  expect_identical(df, res)
})

test_that("select handles grouped tibble with 0 groups (#3935)", {
  df <- tibble(x=integer()) %>% group_by(x)
  res <- select(df, x)
  expect_identical(df, res)
})

test_that("arrange handles grouped tibble with 0 groups (#3935)", {
  df <- tibble(x=integer()) %>% group_by(x)
  res <- arrange(df, x)
  expect_identical(df, res)
})

test_that("group_by() with empty spec produces a grouped data frame with 0 grouping variables", {
  gdata <- group_data(group_by(iris))
  expect_equal(names(gdata), ".rows")
  expect_equal(gdata$.rows, list_of(1:nrow(iris)))

  gdata <- group_data(group_by(iris, !!!list()))
  expect_equal(names(gdata), ".rows")
  expect_equal(gdata$.rows, list_of(1:nrow(iris)))
})

# .drop = TRUE ---------------------------------------------------

test_that("group_by(.drop = TRUE) drops empty groups (4061)", {
  res <- iris %>%
    filter(Species == "setosa") %>%
    group_by(Species, .drop = TRUE)

  expect_identical(
    group_data(res),
    structure(
      tibble(Species = factor("setosa", levels = levels(iris$Species)), .rows := list_of(1:50)),
      .drop = TRUE
    )
  )

  expect_true(group_by_drop_default(res))
})

test_that("grouped data frames remember their .drop (#4061)", {
  res <- iris %>%
    filter(Species == "setosa") %>%
    group_by(Species, .drop = TRUE)

  res2 <- res %>%
    filter(Sepal.Length > 5)
  expect_true(group_by_drop_default(res2))

  res3 <- res %>%
    filter(Sepal.Length > 5, .preserve = FALSE)
  expect_true(group_by_drop_default(res3))

  res4 <- res3 %>%
    group_by(Species)
  expect_true(group_by_drop_default(res4))
  expect_equal(nrow(group_data(res4)), 1L)
})

test_that("grouped data frames remember their .drop = FALSE (#4337)", {
  res <- iris %>%
    filter(Species == "setosa") %>%
    group_by(Species, .drop = FALSE)
  expect_false(group_by_drop_default(res))

  res2 <- res %>%
    group_by(Species)
  expect_false(group_by_drop_default(res2))
})

test_that("summarise maintains the .drop attribute (#4061)", {
  df <- tibble(
    f1 = factor("a", levels = c("a", "b", "c")),
    f2 = factor("d", levels = c("d", "e", "f", "g")),
    x  = 42
  )

  res <- df %>%
    group_by(f1, f2, .drop = TRUE)
  expect_equal(n_groups(res), 1L)

  res2 <- summarise(res, x = sum(x))
  expect_equal(n_groups(res2), 1L)
  expect_true(group_by_drop_default(res2))
})

test_that("joins maintain the .drop attribute (#4061)", {
  df1 <- group_by(tibble(
    f1 = factor(c("a", "b"), levels = c("a", "b", "c")),
    x  = 42:43
  ), f1, .drop = TRUE)

  df2 <- group_by(tibble(
    f1 = factor(c("a"), levels = c("a", "b", "c")),
    y = 1
  ), f1, .drop = TRUE)

  res <- left_join(df1, df2)
  expect_equal(n_groups(res), 2L)

  df2 <- group_by(tibble(
    f1 = factor(c("a", "c"), levels = c("a", "b", "c")),
    y = 1:2
  ), f1, .drop = TRUE)
  res <- full_join(df1, df2)
  expect_equal(n_groups(res), 3L)
})

test_that("group_by(add = TRUE) sets .drop if the origonal data was .drop", {
  d <- tibble(
    f1 = factor("b", levels = c("a", "b", "c")),
    f2 = factor("g", levels = c("e", "f", "g")),
    x  = 48
  )

  res <- group_by(group_by(d, f1, .drop = TRUE), f2, .add = TRUE)
  expect_equal(n_groups(res), 1L)
  expect_true(group_by_drop_default(res))
})

test_that("group_by_drop_default() is forgiving about corrupt grouped df (#4306)",{
  df <- tibble(x = 1:2, y = 1:2) %>%
    structure(class = c("grouped_df", "tbl_df", "tbl", "data.frame"))

  expect_true(group_by_drop_default(df))
})

test_that("group_by() puts NA groups last in STRSXP (#4227)", {
  res <- tibble(x = c("apple", NA, "banana"), y = 1:3) %>%
    group_by(x) %>%
    group_data()
  expect_identical(res$x, c("apple", "banana", NA_character_))
  expect_identical(res$.rows, list_of(1L, 3L, 2L))
})

test_that("group_by() does not create arbitrary NA groups for factors when drop = TRUE (#4460)", {
  res <- expect_warning(group_data(group_by(iris, Species)[0, ]), NA)
  expect_equal(nrow(res), 0L)

  res <- expect_warning(group_data(group_by(iris[0, ], Species)), NA)
  expect_equal(nrow(res), 0L)
})

test_that("group_by() can handle auto splicing in the mutate() step", {
  expect_identical(
    iris %>% group_by(Species),
    iris %>% group_by(data.frame(Species = Species))
  )

  expect_identical(
    iris %>% group_by(Species),
    iris %>% group_by(across(Species))
  )

  expect_identical(
    iris %>% mutate(across(starts_with("Sepal"), round)) %>% group_by(Sepal.Length, Sepal.Width),
    iris %>% group_by(across(starts_with("Sepal"), round))
  )

})

test_that("group_by() can combine usual spec and auto-splicing-mutate() step", {
  expect_identical(
    iris %>% mutate(across(starts_with("Sepal"), round)) %>% group_by(Sepal.Length, Sepal.Width, Species),
    iris %>% group_by(across(starts_with("Sepal"), round), Species)
  )

  expect_identical(
    iris %>% mutate(across(starts_with("Sepal"), round)) %>% group_by(Species, Sepal.Length, Sepal.Width),
    iris %>% group_by(Species, across(starts_with("Sepal"), round))
  )
})

# mutate() semantics

test_that("group_by() has mutate() semantics (#4984)", {
  expect_equal(
    tibble(a = 1, b = 2) %>% group_by(c = a * b, d = c + 1),
    tibble(a = 1, b = 2) %>% mutate(c = a * b, d = c + 1) %>% group_by(c, d)
  )
})

# Errors ------------------------------------------------------------------

test_that("group_by() and ungroup() give meaningful error messages", {
  verify_output(test_path("test-group-by-errors.txt"), {
    df <- tibble(x = 1, y = 2)

    df %>% group_by(unknown)

    df %>% ungroup(x)
    df %>% group_by(x, y) %>% ungroup(z)
  })
})

