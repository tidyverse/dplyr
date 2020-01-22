context("Summarise")

test_that("repeated outputs applied progressively", {
  df <- data.frame(x = 5)

  out <- summarise(df, x = mean(x), x = x + 1)
  expect_equal(nrow(out), 1)
  expect_equal(ncol(out), 1)

  expect_equal(out$x, 6)
})

test_that("repeated outputs applied progressively (grouped_df)", {
  df <- data.frame(x = c(1, 1), y = 1:2)
  ds <- group_by(df, y)
  out <- summarise(ds, z = mean(x), z = z + 1)

  expect_equal(nrow(out), 2)
  expect_equal(ncol(out), 2)

  expect_equal(out$z, c(2L, 2L))
})


test_that("summarise peels off a single layer of grouping", {
  df <- tibble(x = rep(1:4, each = 4), y = rep(1:2, each = 8), z = runif(16))
  grouped <- df %>% group_by(x, y, z)
  expect_equal(group_vars(grouped), c("x", "y", "z"))
  expect_equal(group_vars(grouped %>% summarise(n = n())), c("x", "y"))
})

test_that("summarise can refer to variables that were just created (#138)", {
  res <- summarise(mtcars, cyl1 = mean(cyl), cyl2 = cyl1 + 1)
  expect_equal(res$cyl2, mean(mtcars$cyl) + 1)

  gmtcars <- group_by(mtcars, am)
  res <- summarise(gmtcars, cyl1 = mean(cyl), cyl2 = cyl1 + 1)
  res_direct <- summarise(gmtcars, cyl2 = mean(cyl) + 1)
  expect_equal(res$cyl2, res_direct$cyl2)
})

test_that("summarise can modify grouping variables", {
  df <- tibble(a = c(1, 2, 1, 2), b = c(1, 1, 2, 2))
  gf <- group_by(df, a, b)

  i <- count_regroups(out <- summarise(gf, a = a + 1))
  expect_equal(i, 1)
  expect_equal(out$a, c(2, 2, 3, 3))
})

test_that("summarise handles passing ...", {
  df <- data.frame(x = 1:4)

  f <- function(...) {
    x1 <- 1
    f1 <- function(x) x
    summarise(df, ..., x1 = f1(x1))
  }
  g <- function(...) {
    x2 <- 2
    f(x2 = x2, ...)
  }
  h <- function(before = "before", ..., after = "after") {
    g(before = before, ..., after = after)
  }

  res <- h(x3 = 3)
  expect_equal(res$x1, 1)
  expect_equal(res$x2, 2)
  expect_equal(res$before, "before")
  expect_equal(res$after, "after")

  df <- as_tibble(df)
  res <- h(x3 = 3)
  expect_equal(res$x1, 1)
  expect_equal(res$x2, 2)
  expect_equal(res$before, "before")
  expect_equal(res$after, "after")

  df <- group_by(df, x)
  res <- h(x3 = 3)
  expect_equal(res$x1, rep(1, 4))
  expect_equal(res$x2, rep(2, 4))
  expect_equal(res$before, rep("before", 4))
  expect_equal(res$after, rep("after", 4))
})

test_that("summarise allows names (#2675)", {
  data <- tibble(a = 1:3) %>% summarise(b = c("1" = a[[1]]))
  expect_equal(names(data$b), "1")

  data <- tibble(a = 1:3) %>% rowwise() %>% summarise(b = setNames(nm = a))
  expect_equal(names(data$b), c("1", "2", "3"))

  data <- tibble(a = c(1, 1, 2)) %>% group_by(a) %>% summarise(b = setNames(nm = a[[1]]))
  expect_equal(names(data$b), c("1", "2"))

  res <- data.frame(x = c(1:3), y = letters[1:3]) %>%
    group_by(y) %>%
    summarise(
      a = length(x),
      b = quantile(x, 0.5)
    )
  expect_equal(res$b, c("50%" = 1, "50%" = 2, "50%" = 3))
})

test_that("summarise creates an empty data frame with one row when no parameters are used", {
  res <- summarise(mtcars)
  expect_equal(nrow(res), 1L)
})

test_that("summarise works with zero-row data frames", {
  res <- summarise(mtcars[0, ], n = n(), sum = sum(cyl), mean = mean(mpg), var = var(drat))
  expect_equal(res, data.frame(n = 0L, sum = 0, mean = NaN, var = NA_real_))
})

test_that("summarise works with zero-column data frames (#3071)", {
  res <- summarise(mtcars[0], n = n())
  expect_equal(res, data.frame(n = nrow(mtcars)))
})

test_that("comment attribute is allowed (#346)", {
  test <- data.frame(A = c(1, 1, 0, 0), B = c(2, 2, 3, 3))
  comment(test$B) <- "2nd Var"
  res <- group_by(test, A)
  expect_equal(comment(res$B), "2nd Var")
})

test_that("summarise is not polluted by logical NA (#599)", {
  dat <- data.frame(grp = rep(1:4, each = 2), val = c(NA, 2, 3:8))
  Mean <- function(x, thresh = 2) {
    res <- mean(x, na.rm = TRUE)
    if (res > thresh) res else NA
  }
  res <- dat %>% group_by(grp) %>% summarise(val = Mean(val, thresh = 2))
  expect_is(res$val, "numeric")
  expect_true(is.na(res$val[1]))
})

test_that("summarise handles list output columns (#832)", {
  df <- tibble(x = 1:10, g = rep(1:2, each = 5))
  res <- df %>% group_by(g) %>% summarise(y = list(x))
  expect_equal(res$y[[1]], 1:5)
  expect_equal(res$y[[2]], 6:10)

  res <- df %>% group_by(g) %>% summarise(y = list(x + 1))
  expect_equal(res$y[[1]], 1:5 + 1)
  expect_equal(res$y[[2]], 6:10 + 1)

  df <- tibble(x = 1:10, g = rep(1:2, each = 5))
  res <- df %>% summarise(y = list(x))
  expect_equal(res$y[[1]], 1:10)
  res <- df %>% summarise(y = list(x + 1))
  expect_equal(res$y[[1]], 1:10 + 1)
})

test_that("summarise works with empty data frame (#1142)", {
  df <- data.frame()
  res <- df %>% summarise()
  expect_equal(nrow(res), 1L)
  expect_equal(length(res), 0L)
})

test_that("summarise handles promotion of results (#893)", {
  df <- structure(
    list(
      price = c(
        580L, 650L, 630L, 706L, 1080L, 3082L, 3328L, 4229L, 1895L,
        3546L, 752L, 13003L, 814L, 6115L, 645L, 3749L, 2926L, 765L,
        1140L, 1158L
      ),
      cut = structure(c(
        2L, 4L, 4L, 2L, 3L, 2L, 2L, 3L, 4L, 1L, 1L, 3L, 2L,
        4L, 3L, 3L, 1L, 2L, 2L, 2L
      ),
      .Label = c("Good", "Ideal", "Premium", "Very Good"),
      class = "factor"
      )
    ),
    row.names = c(NA, -20L),
    .Names = c("price", "cut"),
    class = "data.frame"
  )
  res <- df %>%
    group_by(cut) %>%
    select(price) %>%
    summarise(price = median(price))
  expect_is(res$price, "numeric")
})

test_that("summarise correctly handles logical (#1291)", {
  test <- expand.grid(id = 1:2, type = letters[1:2], sample = 1:2) %>%
    mutate(var = c(1, 0, 1, 1, 0, 0, 0, 1)) %>%
    mutate(var_l = as.logical(var)) %>%
    mutate(var_ch = as.character(var_l)) %>%
    arrange(id, type, sample) %>%
    group_by(id, type)
  test_sum <- test %>%
    ungroup() %>%
    group_by(id, type) %>%
    summarise(
      anyvar = any(var == 1),
      anyvar_l = any(var_l),
      anyvar_ch = any(var_ch == "TRUE")
    )

  expect_equal(test_sum$anyvar, c(TRUE, TRUE, FALSE, TRUE))
})

test_that("summarise correctly handles NA groups (#1261)", {
  tmp <- tibble(
    a = c(1, 1, 1, 2, 2),
    b1 = NA_integer_,
    b2 = NA_character_
  )

  res <- tmp %>% group_by(a, b1) %>% summarise(n = n())
  expect_equal(nrow(res), 2L)
  res <- tmp %>% group_by(a, b2) %>% summarise(n = n())
  expect_equal(nrow(res), 2L)
})

test_that("data.frame columns are supported in summarise (#1425)", {
  df <- data.frame(x1 = rep(1:3, times = 3), x2 = 1:9)
  df$x3 <- df %>% mutate(x3 = x2)
  res <- df %>% group_by(x1) %>% summarise(nr = nrow(x3))
  expect_true(all(res$nr == 3))
})

test_that("group_by keeps classes (#1631)", {
  df <- data.frame(a = 1, b = as.Date(NA)) %>%
    group_by(a) %>%
    summarize(c = min(b))
  expect_equal(class(df$c), "Date")

  df <- data.frame(a = 1, b = as.POSIXct(NA)) %>%
    group_by(a) %>%
    summarize(c = min(b))
  expect_equal(class(df$c), c("POSIXct", "POSIXt"))
})

test_that("summarise() correctly coerces factors with different levels (#1678)", {
  res <- tibble(x = 1:3) %>%
    group_by(x) %>%
    summarise(
      y = if (x == 1) "a" else "b",
      z = factor(y)
    )
  expect_is(res$z, "factor")
  expect_equal(levels(res$z), c("a", "b"))
  expect_equal(as.character(res$z), c("a", "b", "b"))
})

test_that("summarise handles raw columns (#1803)", {
  df <- tibble(a = 1:3, b = as.raw(1:3))
  expect_equal(summarise(df, c = sum(a)), tibble(c = 6L))
  expect_identical(summarise(df, c = b[[1]]), tibble(c = as.raw(1)))
})

test_that("summarise supports matrix columns", {
  df <- data.frame(a = 1:3, b = 1:3)

  df_regular <- summarise(df, b = scale(b))
  df_grouped <- summarise(group_by(df, a), b = scale(b))
  df_rowwise <- summarise(rowwise(df), b = scale(b))

  expect_equal(dim(df_regular$b), c(3, 1))
  expect_equal(dim(df_grouped$b), c(3, 1))
  expect_equal(dim(df_rowwise$b), c(3, 1))
})

test_that("typing and NAs for grouped summarise (#1839)", {
  expect_identical(
    tibble(id = 1L, a = NA_character_) %>%
      group_by(id) %>%
      summarise(a = a[[1]]) %>%
      .$a,
    NA_character_
  )

  expect_identical(
    tibble(id = 1:2, a = c(NA, "a")) %>%
      group_by(id) %>%
      summarise(a = a[[1]]) %>%
      .$a,
    c(NA, "a")
  )

  # Properly upgrade NA (logical) to character
  expect_identical(
    tibble(id = 1:2, a = 1:2) %>%
      group_by(id) %>%
      summarise(a = ifelse(all(a < 2), NA, "yes")) %>%
      .$a,
    c(NA, "yes")
  )

  expect_identical(
    tibble(id = 1:2, a = list(1, "2")) %>%
      group_by(id) %>%
      summarise(a = a[1]) %>%
      .$a,
    list(1, "2")
  )
})

test_that("typing and NAs for rowwise summarise (#1839)", {
  expect_identical(
    tibble(id = 1L, a = NA_character_) %>%
      rowwise() %>%
      summarise(a = a[[1]]) %>%
      .$a,
    NA_character_
  )

  expect_identical(
    tibble(id = 1:2, a = c(NA, "a")) %>%
      rowwise() %>%
      summarise(a = a[[1]]) %>%
      .$a,
    c(NA, "a")
  )

  # Properly promote NA (logical) to character
  expect_identical(
    tibble(id = 1:2, a = 1:2) %>%
      group_by(id) %>%
      summarise(a = ifelse(all(a < 2), NA, "yes")) %>%
      .$a,
    c(NA, "yes")
  )
})

test_that("ungrouped summarise() uses summary variables correctly (#2404)", {
  df <- tibble(value = seq(1:10))

  out <- df %>% summarise(value = mean(value), sd = sd(value))
  expect_equal(out$value, 5.5)
  expect_equal(out$sd, NA_real_)
})

test_that("proper handling of names in summarised list columns (#2231)", {
  d <- tibble(x = rep(1:3, 1:3), y = 1:6, names = letters[1:6])
  res <- d %>% group_by(x) %>% summarise(y = list(setNames(y, names)))
  expect_equal(names(res$y[[1]]), letters[[1]])
  expect_equal(names(res$y[[2]]), letters[2:3])
  expect_equal(names(res$y[[3]]), letters[4:6])
})

test_that("summarise() supports unquoted values", {
  df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)
  expect_identical(summarise(df, out = !!1), tibble(out = 1))
  expect_identical(summarise(df, out = !!quote(identity(1))), tibble(out = 1))
  expect_equal(summarise(df, out = !!(1:2)), tibble(out = 1:2))

  gdf <- group_by(df, g)
  expect_identical(summarise(gdf, out = !!1), summarise(gdf, out = 1))
  expect_identical(summarise(gdf, out = !!(1:2)), tibble(g = c(1, 1, 2, 2), out = c(1:2, 1:2)))
  expect_identical(summarise(gdf, out = !!quote(identity(1))), summarise(gdf, out = 1))
  expect_equal(summarise(gdf, out = !!(1:5)) %>% nrow(), 10L)
})

test_that("formulas are evaluated in the right environment (#3019)", {
  out <- mtcars %>% summarise(fn = list(rlang::as_function(~ list(~foo, environment()))))
  out <- out$fn[[1]]()
  expect_identical(environment(out[[1]]), out[[2]])
})

test_that("summarise correctly reconstruct group rows", {
  d <- tibble(x = 1:4, g1 = rep(1:2, 2), g2 = 1:4) %>%
    group_by(g1, g2) %>%
    summarise(x = x+1)
  expect_equal(group_rows(d), list_of(1:2, 3:4))
})

test_that("tidy eval does not infloop (#4049)", {
  df <- data.frame(x = 1:5)
  call <- expr(length(!!quo(x)))
  expect_identical(summarise(df, x = eval_tidy(call)), data.frame(x = 5L))
})

test_that("summarise() correctly handle summarised list columns (#4349)", {
  res <- tibble(grp = "grp") %>%
    group_by(grp) %>%
    summarise(z = list(1), y = z)
  expect_identical(res$z, res$y)
  expect_equal(res$z, list(1))
})

test_that("summarise() unpacks unnamed tibble results (#2326)", {
  expect_equal(
    iris %>% group_by(Species) %>% summarise(
      tibble(Sepal = mean(Sepal.Length * Petal.Length), Petal = mean(Petal.Length * Petal.Width))
    ),
    iris %>% group_by(Species) %>% summarise(Sepal = mean(Sepal.Length * Petal.Length), Petal = mean(Petal.Length * Petal.Width))
  )
})

test_that("summarise() packs named tibble results (#2326)", {
  res <- iris %>%
    group_by(Species) %>%
    summarise(
      out = tibble(Sepal = mean(Sepal.Length * Petal.Length), Petal = mean(Petal.Length * Petal.Width))
    )
  expect_is(res$out, "data.frame")
  expect_equal(nrow(res$out), 3L)
})

test_that("summarise() keeps class, but not attributes", {
  df <- structure(
    data.frame(x = 1:10, g1 = rep(1:2, each = 5), g2 = rep(1:5, 2)),
    meta = "this is important"
  )

  out <- df %>% summarise(n = n())
  expect_s3_class(out, "data.frame", exact = TRUE)
  expect_equal(attr(out, "res"), NULL)

  out <- df %>% group_by(g1) %>% summarise(n = n())
  # expect_s3_class(out, "data.frame", exact = TRUE)
  expect_equal(attr(out, "res"), NULL)
})

test_that("summarise() recycles", {
  expect_equal(
    tibble() %>% summarise(x = 1, y = 1:3, z = 1),
    tibble(x = 1, y = 1:3, z = 1)
  )

  expect_equal(
    tibble(a = 1:2) %>% group_by(a) %>% summarise(x = 1, y = 1:3, z = 1),
    tibble(a = rep(1:2, each = 3), x = 1, y = c(1:3, 1:3), z = 1)
  )
  expect_equal(
    tibble(a = 1:2) %>%
      group_by(a) %>%
      summarise(x = seq_len(a), y = 1),
    tibble(a = c(1L, 2L, 2L), x = c(1L, 1L, 2L), y = 1)
  )
})

test_that("summarise() give meaningful errors", {
  verify_output(test_path("test-summarise-errors.txt"), {
    "# unsupported type"
    tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>%
      summarise(a = env(a = 1))
    tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>%
      group_by(x, y) %>%
      summarise(a = env(a = 1))
    tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>%
      rowwise() %>%
      summarise(a = lm(y ~ x))

    "# mixed types"
    tibble(id = 1:2, a = list(1, "2")) %>%
      group_by(id) %>%
      summarise(a = a[[1]])
    tibble(id = 1:2, a = list(1, "2")) %>%
      rowwise() %>%
      summarise(a = a[[1]])

    "# incompatible size"
    tibble(z = 1) %>%
      summarise(x = 1:3, y = 1:2)
    tibble(z = 1:2) %>%
      group_by(z) %>%
      summarise(x = 1:3, y = 1:2)
    tibble(z = 2:1) %>%
      group_by(z) %>%
      summarise(x = seq_len(z), y = 1:2)

    "# Missing variable"
    summarise(mtcars, a = mean(not_there))
    summarise(group_by(mtcars, cyl), a = mean(not_there))

    "# .data pronoun"
    summarise(tibble(a = 1), c = .data$b)
    summarise(group_by(tibble(a = 1:3), a), c = .data$b)
  })
})
