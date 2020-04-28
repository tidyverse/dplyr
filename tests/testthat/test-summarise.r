test_that("can use freshly create variables (#138)", {
  df <- tibble(x = 1:10)
  out <- summarise(df, y = mean(x), z = y + 1)
  expect_equal(out$y, 5.5)
  expect_equal(out$z, 6.5)
})

test_that("inputs are recycled", {
  expect_equal(
    tibble() %>% summarise(x = 1, y = 1:3, z = 1),
    tibble(x = 1, y = 1:3, z = 1)
  )

  gf <- group_by(tibble(a = 1:2), a)
  expect_equal(
    gf %>% summarise(x = 1, y = 1:3, z = 1),
    tibble(a = rep(1:2, each = 3), x = 1, y = c(1:3, 1:3), z = 1)
  )
  expect_equal(
    gf %>% summarise(x = seq_len(a), y = 1),
    tibble(a = c(1L, 2L, 2L), x = c(1L, 1L, 2L), y = 1)
  )
})

test_that("works with empty data frames", {
  # 0 rows
  df <- tibble(x = integer())
  expect_equal(summarise(df), tibble(.rows = 1))
  expect_equal(summarise(df, n = n(), sum = sum(x)), tibble(n = 0, sum = 0))

  # 0 cols
  df <- tibble(.rows = 10)
  expect_equal(summarise(df), tibble(.rows = 1))
  expect_equal(summarise(df, n = n()), tibble(n = 10))
})

test_that("works with grouped empty data frames", {
  df <- tibble(x = integer())

  expect_equal(
    df %>% group_by(x) %>% summarise(y = 1L),
    tibble(x = integer(), y = integer())
  )
  expect_equal(
    df %>% rowwise(x) %>% summarise(y = 1L),
    rowwise(tibble(x = integer(), y = integer()), x)
  )
})

test_that("no expressions yields grouping data", {
  df <- tibble(x = 1:2, y = 1:2)
  gf <- group_by(df, x)

  expect_equal(summarise(df), tibble(.rows = 1))
  expect_equal(summarise(gf), tibble(x = 1:2))

  expect_equal(summarise(df, !!!list()), tibble(.rows = 1))
  expect_equal(summarise(gf, !!!list()), tibble(x = 1:2))
})

test_that("preserved class, but not attributes", {
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

test_that("works with unquoted values", {
  df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)
  expect_equal(summarise(df, out = !!1), tibble(out = 1))
  expect_equal(summarise(df, out = !!quo(1)), tibble(out = 1))
  expect_equal(summarise(df, out = !!(1:2)), tibble(out = 1:2))
})

test_that("formulas are evaluated in the right environment (#3019)", {
  out <- mtcars %>% summarise(fn = list(rlang::as_function(~ list(~foo, environment()))))
  out <- out$fn[[1]]()
  expect_identical(environment(out[[1]]), out[[2]])
})

test_that("data frame results with 0 columns are ignored (#5084)", {
  df1 <- tibble(x = 1:2)
  expect_equal(df1 %>% group_by(x) %>% summarise(data.frame()), df1)
  expect_equal(df1 %>% group_by(x) %>% summarise(data.frame(), y = 65), mutate(df1, y = 65))
  expect_equal(df1 %>% group_by(x) %>% summarise(y = 65, data.frame()), mutate(df1, y = 65))

  df2 <- tibble(x = 1:2, y = 3:4)
  expect_equal(df2 %>% group_by(x) %>% summarise(data.frame()), df1)
  expect_equal(df2 %>% group_by(x) %>% summarise(data.frame(), z = 98), mutate(df1, z = 98))
  expect_equal(df2 %>% group_by(x) %>% summarise(z = 98, data.frame()), mutate(df1, z = 98))
})

# grouping ----------------------------------------------------------------

test_that("peels off a single layer of grouping", {
  df <- tibble(x = rep(1:4, each = 4), y = rep(1:2, each = 8), z = runif(16))
  gf <- df %>% group_by(x, y)
  expect_equal(group_vars(summarise(gf)), "x")
  expect_equal(group_vars(summarise(summarise(gf))), character())
})

test_that("correctly reconstructs groups", {
  d <- tibble(x = 1:4, g1 = rep(1:2, 2), g2 = 1:4) %>%
    group_by(g1, g2) %>%
    summarise(x = x + 1)
  expect_equal(group_rows(d), list_of(1:2, 3:4))
})

test_that("can modify grouping variables", {
  df <- tibble(a = c(1, 2, 1, 2), b = c(1, 1, 2, 2))
  gf <- group_by(df, a, b)

  i <- count_regroups(out <- summarise(gf, a = a + 1))
  expect_equal(i, 1)
  expect_equal(out$a, c(2, 2, 3, 3))
})

# vector types ----------------------------------------------------------

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

test_that("summarise handles list output columns (#832)", {
  df <- tibble(x = 1:10, g = rep(1:2, each = 5))
  res <- df %>% group_by(g) %>% summarise(y = list(x))
  expect_equal(res$y[[1]], 1:5)

  # preserving names
  d <- tibble(x = rep(1:3, 1:3), y = 1:6, names = letters[1:6])
  res <- d %>% group_by(x) %>% summarise(y = list(setNames(y, names)))
  expect_equal(names(res$y[[1]]), letters[[1]])
})

test_that("summarise coerces types across groups", {
  gf <- group_by(tibble(g = 1:2), g)

  out <- summarise(gf, x = if (g == 1) NA else "x")
  expect_type(out$x, "character")

  out <- summarise(gf, x = if (g == 1L) NA else 2.5)
  expect_type(out$x, "double")
})

test_that("unnamed tibbles are unpacked (#2326)", {
  df <- tibble(x = 1:2)
  out <- summarise(df, tibble(y = x * 2, z = 3))
  expect_equal(out$y, c(2L, 4L))
  expect_equal(out$z, c(3L, 3L))
})

test_that("named tibbles are packed (#2326)", {
  df <- tibble(x = 1:2)
  out <- summarise(df, df = tibble(y = x * 2, z = 3))
  expect_equal(out$df, tibble(y = c(2L, 4L), z = c(3L, 3L)))
})

test_that("summarise(.groups=)", {
  expect_message(eval_bare(
    expr(data.frame(x = 1, y = 2) %>% group_by(x, y) %>% summarise()),
    env(global_env())
  ))

  df <- data.frame(x = 1, y = 2)
  gf <- df %>% group_by(x, y)
  expect_equal(gf %>% summarise() %>% group_vars(), "x")
  expect_equal(gf %>% summarise(.groups = "drop_last") %>% group_vars(), "x")
  expect_equal(gf %>% summarise(.groups = "drop") %>% group_vars(), character())
  expect_equal(gf %>% summarise(.groups = "keep") %>% group_vars(), c("x", "y"))

  rf <- df %>% rowwise(x, y)
  expect_equal(rf %>% summarise(.groups = "drop") %>% group_vars(), character())
  expect_equal(rf %>% summarise(.groups = "keep") %>% group_vars(), c("x", "y"))
})

# errors -------------------------------------------------------------------

test_that("summarise() gives meaningful errors", {
  verify_output(test_path("test-summarise-errors.txt"), {
    "# Messages about .groups="
    eval_bare(
      expr(
        tibble(x = 1, y = 2) %>%
          group_by(x, y) %>%
          summarise()
      ),
      env(global_env())
    )

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
    tibble(z = c(1, 3)) %>%
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
