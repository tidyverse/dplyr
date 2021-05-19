# across ------------------------------------------------------------------

test_that("across() works on one column data.frame", {
  df <- data.frame(x = 1)

  out <- df %>% mutate(across())
  expect_equal(out, df)
})

test_that("across() does not select grouping variables", {
  df <- data.frame(g = 1, x = 1)

  out <- df %>% group_by(g) %>% summarise(x = across(everything())) %>% pull()
  expect_equal(out, tibble(x = 1))
})

test_that("across() correctly names output columns", {
  gf <- tibble(x = 1, y = 2, z = 3, s = "") %>% group_by(x)

  expect_named(
    summarise(gf, across()),
    c("x", "y", "z", "s")
  )
  expect_named(
    summarise(gf, across(.names = "id_{.col}")),
    c("x", "id_y", "id_z", "id_s")
  )
  expect_named(
    summarise(gf, across(where(is.numeric), mean)),
    c("x", "y", "z")
  )
  expect_named(
    summarise(gf, across(where(is.numeric), mean, .names = "mean_{.col}")),
    c("x", "mean_y", "mean_z")
  )
  expect_named(
    summarise(gf, across(where(is.numeric), list(mean = mean, sum = sum))),
    c("x", "y_mean", "y_sum", "z_mean", "z_sum")
  )
  expect_named(
    summarise(gf, across(where(is.numeric), list(mean = mean, sum))),
    c("x", "y_mean", "y_2", "z_mean", "z_2")
  )
  expect_named(
    summarise(gf, across(where(is.numeric), list(mean, sum = sum))),
    c("x", "y_1", "y_sum", "z_1", "z_sum")
  )
  expect_named(
    summarise(gf, across(where(is.numeric), list(mean, sum))),
    c("x", "y_1", "y_2", "z_1", "z_2")
  )
  expect_named(
    summarise(gf, across(where(is.numeric), list(mean = mean, sum = sum), .names = "{.fn}_{.col}")),
    c("x", "mean_y", "sum_y", "mean_z", "sum_z")
  )
})

test_that("across() result locations are aligned with column names (#4967)", {
  df <- tibble(x = 1:2, y = c("a", "b"))
  expect <- tibble(x_cls = "integer", x_type = TRUE, y_cls = "character", y_type = FALSE)

  x <- summarise(df, across(everything(), list(cls = class, type = is.numeric)))

  expect_identical(x, expect)
})

test_that("across() passes ... to functions", {
  df <- tibble(x = c(1, NA))
  expect_equal(
    summarise(df, across(everything(), mean, na.rm = TRUE)),
    tibble(x = 1)
  )
  expect_equal(
    summarise(df, across(everything(), list(mean = mean, median = median), na.rm = TRUE)),
    tibble(x_mean = 1, x_median = 1)
  )
})

test_that("across() passes unnamed arguments following .fns as ... (#4965)", {
  df <- tibble(x = 1)
  expect_equal(mutate(df, across(x, `+`, 1)), tibble(x = 2))
})

test_that("across() avoids simple argument name collisions with ... (#4965)", {
  df <- tibble(x = c(1, 2))
  expect_equal(summarize(df, across(x, tail, n = 1)), tibble(x = 2))
})

test_that("across() works sequentially (#4907)", {
  df <- tibble(a = 1)
  expect_equal(
    mutate(df, x = ncol(across(where(is.numeric))), y = ncol(across(where(is.numeric)))),
    tibble(a = 1, x = 1L, y = 2L)
  )
  expect_equal(
    mutate(df, a = "x", y = ncol(across(where(is.numeric)))),
    tibble(a = "x", y = 0L)
  )
  expect_equal(
    mutate(df, x = 1, y = ncol(across(where(is.numeric)))),
    tibble(a = 1, x = 1, y = 2L)
  )
})

test_that("across() retains original ordering", {
  df <- tibble(a = 1, b = 2)
  expect_named(mutate(df, a = 2, x = across())$x, c("a", "b"))
})

test_that("across() gives meaningful messages", {
  expect_snapshot(error = TRUE,
    tibble(x = 1) %>%
      summarise(res = across(where(is.numeric), 42))
  )
  expect_snapshot(error = TRUE, across())
  expect_snapshot(error = TRUE, c_across())
})

test_that("monitoring cache - across() can be used twice in the same expression", {
  df <- tibble(a = 1, b = 2)
  expect_equal(
    mutate(df, x = ncol(across(where(is.numeric))) + ncol(across(a))),
    tibble(a = 1, b = 2, x = 3)
  )
})

test_that("monitoring cache - across() can be used in separate expressions", {
  df <- tibble(a = 1, b = 2)
  expect_equal(
    mutate(df, x = ncol(across(where(is.numeric))), y = ncol(across(a))),
    tibble(a = 1, b = 2, x = 2, y = 1)
  )
})

test_that("monitoring cache - across() usage can depend on the group id", {
  df <- tibble(g = 1:2, a = 1:2, b = 3:4)
  df <- group_by(df, g)

  switcher <- function() {
    if_else(cur_group_id() == 1L, across(a)$a, across(b)$b)
  }

  expect <- df
  expect$x <- c(1L, 4L)

  expect_equal(
    mutate(df, x = switcher()),
    expect
  )
})

test_that("monitoring cache - across() internal cache key depends on all inputs", {
  df <- tibble(g = rep(1:2, each = 2), a = 1:4)
  df <- group_by(df, g)

  expect_identical(
    mutate(df, tibble(x = across(where(is.numeric), mean)$a, y = across(where(is.numeric), max)$a)),
    mutate(df, x = mean(a), y = max(a))
  )
})

test_that("across() rejects non vectors", {
  expect_error(
    data.frame(x = 1) %>% summarise(across(everything(), ~sym("foo")))
  )
})

test_that("across() uses tidy recycling rules", {
  expect_equal(
    data.frame(x = 1, y = 2) %>% summarise(across(everything(), ~rep(42, .))),
    data.frame(x = rep(42, 2), y = rep(42, 2))
  )

  expect_error(
    data.frame(x = 2, y = 3) %>% summarise(across(everything(), ~rep(42, .)))
  )
})

test_that("across(<empty set>) returns a data frame with 1 row (#5204)", {
  df <- tibble(x = 1:42)
  expect_equal(
    mutate(df, across(c(), as.factor)),
    df
  )
  expect_equal(
    mutate(df, y = across(c(), as.factor))$y,
    tibble::new_tibble(list(), nrow = 42)
  )
  mutate(df, {
    res <- across(c(), as.factor)
    expect_equal(nrow(res), 1L)
    res
  })
})

test_that("across(.names=) can use local variables in addition to {col} and {fn}", {
  res <- local({
    prefix <- "MEAN"
    data.frame(x = 42) %>%
      summarise(across(everything(), mean, .names = "{prefix}_{.col}"))
  })
  expect_identical(res, data.frame(MEAN_x = 42))
})

test_that("across() uses environment from the current quosure (#5460)", {
  # If the data frame `y` is selected, causes a subscript conversion
  # error since it is fractional
  df <- data.frame(x = 1, y = 2.4)
  y <- "x"
  expect_equal(df %>% summarise(across(all_of(y), mean)), data.frame(x = 1))
  expect_equal(df %>% mutate(across(all_of(y), mean)), df)
  expect_equal(df %>% filter(if_all(all_of(y), ~ .x < 2)), df)

  # Inherited case
  expect_error(df %>% summarise(local(across(all_of(y), mean))))

  expect_equal(
    df %>% summarise(summarise(cur_data(), across(all_of(y), mean))),
    df %>% summarise(across(all_of(y), mean))
  )
})

test_that("across() sees columns in the recursive case (#5498)", {
  df <- tibble(
    vars = list("foo"),
    data = list(data.frame(foo = 1, bar = 2))
  )

  out <- df %>% mutate(data = purrr::map2(data, vars, ~ {
    .x %>% mutate(across(all_of(.y), ~ NA))
  }))
  exp <- tibble(
    vars = list("foo"),
    data = list(data.frame(foo = NA, bar = 2))
  )
  expect_identical(out, exp)

  out <- df %>% mutate(data = purrr::map2(data, vars, ~ {
    local({
      .y <- "bar"
      .x %>% mutate(across(all_of(.y), ~ NA))
    })
  }))
  exp <- tibble(
    vars = list("foo"),
    data = list(data.frame(foo = 1, bar = NA))
  )
  expect_identical(out, exp)
})

test_that("across() works with empty data frames (#5523)", {
   expect_equal(
     mutate(tibble(), across()),
     tibble()
   )
})

test_that("lambdas in mutate() + across() can use columns", {
  df <- tibble(x = 2, y = 4, z = 8)
  expect_identical(
    df %>% mutate(data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% mutate(across(everything(), ~ .x / y))
  )
  expect_identical(
    df %>% mutate(data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% mutate(+across(everything(), ~ .x / y))
  )

  expect_identical(
    df %>% mutate(data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% mutate(across(everything(), ~ .x / .data$y))
  )
  expect_identical(
    df %>% mutate(data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% mutate(+across(everything(), ~ .x / .data$y))
  )
})

test_that("lambdas in summarise() + across() can use columns", {
  df <- tibble(x = 2, y = 4, z = 8)
  expect_identical(
    df %>% summarise(data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% summarise(across(everything(), ~ .x / y))
  )
  expect_identical(
    df %>% summarise(data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% summarise(+across(everything(), ~ .x / y))
  )

  expect_identical(
    df %>% summarise(data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% summarise(across(everything(), ~ .x / .data$y))
  )
  expect_identical(
    df %>% summarise(data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% summarise(+across(everything(), ~ .x / .data$y))
  )
})

test_that("lambdas in mutate() + across() can use columns in follow up expressions (#5717)", {
  df <- tibble(x = 2, y = 4, z = 8)
  expect_identical(
    df %>% mutate(a = 2, data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% mutate(a = 2, across(c(x, y, z), ~ .x / y))
  )
  expect_identical(
    df %>% mutate(a = 2, data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% mutate(a = 2, +across(c(x, y, z), ~ .x / y))
  )

  expect_identical(
    df %>% mutate(a = 2, data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% mutate(a = 2, across(c(x, y, z), ~ .x / .data$y))
  )
  expect_identical(
    df %>% mutate(a = 2, data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% mutate(a = 2, +across(c(x, y, z), ~ .x / .data$y))
  )
})

test_that("lambdas in summarise() + across() can use columns in follow up expressions (#5717)", {
  df <- tibble(x = 2, y = 4, z = 8)
  expect_identical(
    df %>% summarise(a = 2, data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% summarise(a = 2, across(c(x, y, z), ~ .x / y))
  )
  expect_identical(
    df %>% summarise(a = 2, data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% summarise(a = 2, +across(c(x, y, z), ~ .x / y))
  )

  expect_identical(
    df %>% summarise(a = 2, data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% summarise(a = 2, across(c(x, y, z), ~ .x / .data$y))
  )
  expect_identical(
    df %>% summarise(a = 2, data.frame(x = x / y, y = y / y, z = z / y)),
    df %>% summarise(a = 2, +across(c(x, y, z), ~ .x / .data$y))
  )
})

test_that("functions defined inline can use columns (#5734)", {
  df <- data.frame(x = 1, y = 2)
  expect_equal(
    df %>% mutate(across('x', function(.x) .x / y)) %>% pull(x),
    0.5
  )
})

test_that("if_any() and if_all() do not enforce logical", {
  # We used to coerce to logical using vctrs. Now we use base
  # semantics because we expand `if_all(x:y)` to `x & y`.
  d <- data.frame(x = 10, y = 10)
  expect_equal(filter(d, if_all(x:y, identity)), d)
  expect_equal(filter(d, if_any(x:y, identity)), d)

  expect_equal(
    mutate(d, ok = if_any(x:y, identity)),
    mutate(d, ok = TRUE)
  )
  expect_equal(
    mutate(d, ok = if_all(x:y, identity)),
    mutate(d, ok = TRUE)
  )
})

test_that("if_any() and if_all() can be used in mutate() (#5709)", {
  d <- data.frame(x = c(1, 5, 10, 10), y = c(0, 0, 0, 10), z = c(10, 5, 1, 10))
  res <- d %>%
    mutate(
      any = if_any(x:z, ~ . > 8),
      all = if_all(x:z, ~ . > 8)
    )
  expect_equal(res$any, c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(res$all, c(FALSE, FALSE, FALSE, TRUE))
})

test_that("across() caching not confused when used from if_any() and if_all() (#5782)", {
  res <- data.frame(x = 1:3) %>%
    mutate(
      any = if_any(x, ~ . >= 2) + if_any(x, ~ . >= 3),
      all = if_all(x, ~ . >= 2) + if_all(x, ~ . >= 3)
    )
  expect_equal(res$any, c(0, 1, 2))
  expect_equal(res$all, c(0, 1, 2))
})

test_that("if_any() and if_all() respect filter()-like NA handling", {
  df <- expand.grid(
    x = c(TRUE, FALSE, NA), y = c(TRUE, FALSE, NA)
  )
  expect_identical(
    filter(df, x & y),
    filter(df, if_all(c(x,y), identity))
  )
  expect_identical(
    filter(df, x | y),
    filter(df, if_any(c(x,y), identity))
  )
})

test_that("if_any() and if_all() aborts when predicate mistakingly used in .cols= (#5732)", {
  df <- data.frame(x = 1:10, y = 1:10)
  expect_snapshot({
    # expanded case
    (expect_error(filter(df, if_any(~ .x > 5))))
    (expect_error(filter(df, if_all(~ .x > 5))))

    # non expanded case
    (expect_error(filter(df, !if_any(~ .x > 5))))
    (expect_error(filter(df, !if_all(~ .x > 5))))
  })
})

test_that("across() correctly reset column", {
  expect_error(cur_column())
  res <- data.frame(x = 1) %>%
    summarise(
      a = { expect_error(cur_column()); 2},
      across(x, ~{ expect_equal(cur_column(), "x"); 3}, .names = "b"),        # top_across()
      c = { expect_error(cur_column()); 4},
      force(across(x, ~{ expect_equal(cur_column(), "x"); 5}, .names = "d")),  # across()
      e = { expect_error(cur_column()); 6}
    )
  expect_equal(res, data.frame(a = 2, b = 3, c = 4, d = 5, e = 6))
  expect_error(cur_column())

  res <- data.frame(x = 1) %>%
    mutate(
      a = { expect_error(cur_column()); 2},
      across(x, ~{ expect_equal(cur_column(), "x"); 3}, .names = "b"),        # top_across()
      c = { expect_error(cur_column()); 4},
      force(across(x, ~{ expect_equal(cur_column(), "x"); 5}, .names = "d")),  # across()
      e = { expect_error(cur_column()); 6}
    )
  expect_equal(res, data.frame(x = 1, a = 2, b = 3, c = 4, d = 5, e = 6))
  expect_error(cur_column())
})

test_that("top_across() evaluates ... with promise semantics (#5813)", {
  df <- tibble(x = tibble(foo = 1), y = tibble(foo = 2))

  res <- mutate(df, across(
    everything(),
    mutate,
    foo = foo + 1
  ))
  expect_equal(res$x$foo, 2)
  expect_equal(res$y$foo, 3)

  # Can omit dots
  res <- mutate(df, across(
    everything(),
    list
  ))
  expect_equal(res$x[[1]]$foo, 1)
  expect_equal(res$y[[1]]$foo, 2)

  # Dots are evaluated only once
  new_counter <- function() {
    n <- 0L
    function() {
      n <<- n + 1L
      n
    }
  }
  counter <- new_counter()
  list_second <- function(...) {
    list(..2)
  }
  res <- mutate(df, across(
    everything(),
    list_second,
    counter()
  ))
  expect_equal(res$x[[1]], 1)
  expect_equal(res$y[[1]], 1)
})

test_that("group variables are in scope (#5832)", {
  f <- function(x, z) x + z
  gdf <- data.frame(x = 1:2, y = 3:4, g = 1:2) %>% group_by(g)
  exp <- gdf %>% summarise(x = f(x, z = y))

  expect_equal(
    gdf %>% summarise(across(x, ~ f(.x, z = y))),
    exp
  )

  expect_equal(
    gdf %>% summarise(across(x, f, z = y)),
    exp
  )

  expect_equal(
    gdf %>% summarise((across(x, ~ f(.x, z = y)))),
    exp
  )

  expect_equal(
    gdf %>% summarise((across(x, f, z = y))),
    exp
  )
})

test_that("arguments in dots are evaluated once per group", {
  set.seed(0)
  out <- data.frame(g = 1:3, var = NA) %>%
    group_by(g) %>%
    mutate(across(var, function(x, y) y, rnorm(1))) %>%
    pull(var)

  set.seed(0)
  expect_equal(out, rnorm(3))
})

test_that("can pass quosure through `across()`", {
  summarise_mean <- function(data, vars) {
    data %>% summarise(across({{ vars }}, mean))
  }
  gdf <- data.frame(g = c(1, 1, 2), x = 1:3) %>% group_by(g)

  expect_equal(
    gdf %>% summarise_mean(where(is.numeric)),
    summarise(gdf, x = mean(x))
  )
})

test_that("across() inlines formulas", {
  env <- env()
  f <- ~ toupper(.x)

  expect_equal(
    as_across_fn_call(f, quote(foo), env),
    new_quosure(quote(toupper(foo)), f_env(f))
  )

  f <- ~ list(.x, ., .x)
  expect_equal(
    as_across_fn_call(f, quote(foo), env),
    new_quosure(quote(list(foo, foo, foo)), f_env(f))
  )
})

test_that("across() uses local formula environment (#5881)", {
  f <- local({
    prefix <- "foo"
    ~ paste(prefix, .x)
  })
  df <- tibble(x = "x")
  expect_equal(
    mutate(df, across(x, f)),
    tibble(x = "foo x")
  )
})

test_that("unevaluated formulas (currently) fail", {
  df <- tibble(x = "x")
  expect_error(
    mutate(df, across(x, quote(~ paste("foo", .x))))
  )
})

test_that("across() can access lexical scope (#5862)", {
  f_across <- function(data, cols, fn) {
    data %>%
      summarise(
        across({{ cols }}, fn)
      )
  }

  df <- data.frame(x = 1:10, y = 1:10)
  expect_equal(
    f_across(df, c(x, y), mean),
    summarise(df, across(c(x, y), mean))
  )
})

test_that("if_any() and if_all() expansions deal with no inputs or single inputs", {
  d <- data.frame(x = 1)

  # No inputs
  expect_equal(
    filter(d, if_any(starts_with("c"), ~ FALSE)),
    filter(d)
  )
  expect_equal(
    filter(d, if_all(starts_with("c"), ~ FALSE)),
    filter(d)
  )

  # Single inputs
  expect_equal(
    filter(d, if_any(x, ~ FALSE)),
    filter(d, FALSE)
  )
  expect_equal(
    filter(d, if_all(x, ~ FALSE)),
    filter(d, FALSE)
  )
})

test_that("if_any() and if_all() wrapped deal with no inputs or single inputs", {
  d <- data.frame(x = 1)

  # No inputs
  expect_equal(
    filter(d, (if_any(starts_with("c"), ~ FALSE))),
    filter(d)
  )
  expect_equal(
    filter(d, (if_all(starts_with("c"), ~ FALSE))),
    filter(d)
  )

  # Single inputs
  expect_equal(
    filter(d, (if_any(x, ~ FALSE))),
    filter(d, FALSE)
  )
  expect_equal(
    filter(d, (if_all(x, ~ FALSE))),
    filter(d, FALSE)
  )
})


# c_across ----------------------------------------------------------------

test_that("selects and combines columns", {
  df <- data.frame(x = 1:2, y = 3:4)
  out <- df %>% summarise(z = list(c_across(x:y)))
  expect_equal(out$z, list(1:4))
})

test_that("key_deparse() collapses (#5883)", {
  expect_equal(
    length(key_deparse(quo(all_of(c("aaaaaaaaaaaaaaaaaa", "bbbbbbbbbbbbbbb", "cccccccccccccc", "ddddddddddddddddddd"))))),
    1
  )
})
