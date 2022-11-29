test_that("can use freshly create variables (#138)", {
  df <- tibble(x = 1:10)
  out <- summarise(df, y = mean(x), z = y + 1)
  expect_equal(out$y, 5.5)
  expect_equal(out$z, 6.5)
})

test_that("inputs are recycled (deprecated in 1.1.0)", {
  local_options(lifecycle_verbosity = "quiet")

  expect_equal(
    tibble() %>% summarise(x = 1, y = 1:3, z = 1),
    tibble(x = 1, y = 1:3, z = 1)
  )

  gf <- group_by(tibble(a = 1:2), a)
  expect_equal(
    gf %>% summarise(x = 1, y = 1:3, z = 1),
    tibble(a = rep(1:2, each = 3), x = 1, y = c(1:3, 1:3), z = 1) %>% group_by(a)
  )
  expect_equal(
    gf %>% summarise(x = seq_len(a), y = 1),
    tibble(a = c(1L, 2L, 2L), x = c(1L, 1L, 2L), y = 1) %>% group_by(a)
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
    group_by(tibble(x = integer(), y = integer()), x)
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
  expect_null(attr(out, "res"))

  out <- df %>% group_by(g1) %>% summarise(n = n())
  # expect_s3_class(out, "data.frame", exact = TRUE)
  expect_null(attr(out, "res"))
})

test_that("works with unquoted values", {
  df <- tibble(g = c(1, 1, 2, 2, 2), x = 1:5)
  expect_equal(summarise(df, out = !!1), tibble(out = 1))
  expect_equal(summarise(df, out = !!quo(1)), tibble(out = 1))
})

test_that("formulas are evaluated in the right environment (#3019)", {
  out <- mtcars %>% summarise(fn = list(rlang::as_function(~ list(~foo, environment()))))
  out <- out$fn[[1]]()
  expect_identical(environment(out[[1]]), out[[2]])
})

test_that("unnamed data frame results with 0 columns are ignored (#5084)", {
  df1 <- tibble(x = 1:2)
  expect_equal(df1 %>% group_by(x) %>% summarise(data.frame()), df1)
  expect_equal(df1 %>% group_by(x) %>% summarise(data.frame(), y = 65), mutate(df1, y = 65))
  expect_equal(df1 %>% group_by(x) %>% summarise(y = 65, data.frame()), mutate(df1, y = 65))

  df2 <- tibble(x = 1:2, y = 3:4)
  expect_equal(df2 %>% group_by(x) %>% summarise(data.frame()), df1)
  expect_equal(df2 %>% group_by(x) %>% summarise(data.frame(), z = 98), mutate(df1, z = 98))
  expect_equal(df2 %>% group_by(x) %>% summarise(z = 98, data.frame()), mutate(df1, z = 98))

  # This includes unnamed data frames that have 0 columns but >0 rows.
  # Noted when working on (#6509).
  empty3 <- new_tibble(list(), nrow = 3L)
  expect_equal(df1 %>% summarise(empty3), new_tibble(list(), nrow = 1L))
  expect_equal(df1 %>% summarise(empty3, y = mean(x)), df1 %>% summarise(y = mean(x)))
  expect_equal(df1 %>% group_by(x) %>% summarise(empty3), df1)
  expect_equal(df1 %>% group_by(x) %>% summarise(empty3, y = x + 1), mutate(df1, y = x + 1))
})

test_that("named data frame results with 0 columns participate in recycling (#6509)", {
  local_options(lifecycle_verbosity = "quiet")

  df <- tibble(x = 1:3)
  gdf <- group_by(df, x)

  empty <- tibble()
  expect_identical(summarise(df, empty = empty), tibble(empty = empty))
  expect_identical(summarise(df, x = sum(x), empty = empty), tibble(x = integer(), empty = empty))
  expect_identical(summarise(df, empty = empty, x = sum(x)), tibble(empty = empty, x = integer()))

  empty3 <- new_tibble(list(), nrow = 3L)
  expect_identical(summarise(df, empty = empty3), tibble(empty = empty3))
  expect_identical(summarise(df, x = sum(x), empty = empty3), tibble(x = c(6L, 6L, 6L), empty = empty3))
  expect_identical(summarise(df, empty = empty3, x = sum(x)), tibble(empty = empty3, x = c(6L, 6L, 6L)))

  expect_identical(
    summarise(gdf, empty = empty, .groups = "drop"),
    tibble(x = integer(), empty = empty)
  )
  expect_identical(
    summarise(gdf, y = x + 1L, empty = empty, .groups = "drop"),
    tibble(x = integer(), y = integer(), empty = empty)
  )
  expect_identical(
    summarise(gdf, empty = empty3, .groups = "drop"),
    tibble(x = vec_rep_each(1:3, 3), empty = vec_rep(empty3, 3))
  )
  expect_identical(
    summarise(gdf, y = x + 1L, empty = empty3, .groups = "drop"),
    tibble(x = vec_rep_each(1:3, 3), y = vec_rep_each(2:4, 3), empty = vec_rep(empty3, 3))
  )
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

test_that("summarise returns a row for zero length groups", {
  df <- tibble(
    e = 1,
    f = factor(c(1, 1, 2, 2), levels = 1:3),
    g = c(1, 1, 2, 2),
    x = c(1, 2, 1, 4)
  )
  df <- group_by(df, e, f, g, .drop = FALSE)

  expect_equal( nrow(summarise(df, z = n())), 3L)
})

test_that("summarise respects zero-length groups (#341)", {
  df <- tibble(x = factor(rep(1:3, each = 10), levels = 1:4))

  out <- df %>%
    group_by(x, .drop = FALSE) %>%
    summarise(n = n())

  expect_equal(out$n, c(10L, 10L, 10L, 0L))
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
  df <- tibble(x = 2)
  out <- summarise(df, tibble(y = x * 2, z = 3))
  expect_equal(out$y, 4)
  expect_equal(out$z, 3)
})

test_that("named tibbles are packed (#2326)", {
  df <- tibble(x = 2)
  out <- summarise(df, df = tibble(y = x * 2, z = 3))
  expect_equal(out$df, tibble(y = 4, z = 3))
})

test_that("summarise(.groups=)", {
  expect_message(eval_bare(
    expr(data.frame(x = 1, y = 2) %>% group_by(x, y) %>% summarise()),
    env(global_env())
  ))
  expect_message(eval_bare(
    expr(data.frame(x = 1, y = 2) %>% rowwise(x, y) %>% summarise()),
    env(global_env())
  ))

  df <- data.frame(x = 1, y = 2)
  expect_equal(df %>% summarise(z = 3, .groups= "rowwise"), rowwise(data.frame(z = 3)))

  gf <- df %>% group_by(x, y)
  expect_equal(gf %>% summarise() %>% group_vars(), "x")
  expect_equal(gf %>% summarise(.groups = "drop_last") %>% group_vars(), "x")
  expect_equal(gf %>% summarise(.groups = "drop") %>% group_vars(), character())
  expect_equal(gf %>% summarise(.groups = "keep") %>% group_vars(), c("x", "y"))

  rf <- df %>% rowwise(x, y)
  expect_equal(rf %>% summarise(.groups = "drop") %>% group_vars(), character())
  expect_equal(rf %>% summarise(.groups = "keep") %>% group_vars(), c("x", "y"))
})

test_that("summarise() casts data frame results to common type (#5646)", {
  df <- data.frame(x = 1:2, g = 1:2) %>% group_by(g)

  res <- df %>%
    summarise(if (g == 1) data.frame(y = 1) else data.frame(y = 1, z = 2), .groups = "drop")
  expect_equal(res$z, c(NA, 2))
})

test_that("summarise() silently skips when all results are NULL (#5708)", {
  df <- data.frame(x = 1:2, g = 1:2) %>% group_by(g)

  expect_equal(summarise(df, x = NULL), summarise(df))
  expect_error(summarise(df, x = if(g == 1) 42))
})

# .by ----------------------------------------------------------------------

test_that("can group transiently using `.by`", {
  df <- tibble(g = c(1, 1, 2, 1, 2), x = c(5, 2, 1, 2, 3))

  out <- summarise(df, x = mean(x), .by = g)

  expect_identical(out$g, c(1, 2))
  expect_identical(out$x, c(3, 2))
  expect_s3_class(out, class(df), exact = TRUE)
})

test_that("transient grouping retains bare data.frame class", {
  df <- data.frame(g = c(1, 1, 2, 1, 2), x = c(5, 2, 1, 2, 3))
  out <- summarise(df, x = mean(x), .by = g)
  expect_s3_class(out, class(df), exact = TRUE)
})

test_that("transient grouping drops data frame attributes", {
  # Because `summarise()` theoretically creates a "new" data frame

  # With data.frames or tibbles
  df <- data.frame(g = c(1, 1, 2), x = c(1, 2, 1))
  tbl <- as_tibble(df)

  attr(df, "foo") <- "bar"
  attr(tbl, "foo") <- "bar"

  out <- summarise(df, x = mean(x), .by = g)
  expect_null(attr(out, "foo"))

  out <- summarise(tbl, x = mean(x), .by = g)
  expect_null(attr(out, "foo"))
})

test_that("transient grouping orders by first appearance", {
  df <- tibble(g = c(2, 1, 2, 0), x = c(4, 2, 8, 5))

  out <- summarise(df, x = mean(x), .by = g)

  expect_identical(out$g, c(2, 1, 0))
  expect_identical(out$x, c(6, 2, 5))
})

test_that("can't use `.by` with `.groups`", {
  df <- tibble(x = 1)

  expect_snapshot(error = TRUE, {
    summarise(df, .by = x, .groups = "drop")
  })
})

test_that("catches `.by` with grouped-df", {
  df <- tibble(x = 1)
  gdf <- group_by(df, x)

  expect_snapshot(error = TRUE, {
    summarise(gdf, .by = x)
  })
})

test_that("catches `.by` with rowwise-df", {
  df <- tibble(x = 1)
  rdf <- rowwise(df)

  expect_snapshot(error = TRUE, {
    summarise(rdf, .by = x)
  })
})

# errors -------------------------------------------------------------------

test_that("summarise() preserves the call stack on error (#5308)", {
  foobar <- function() stop("foo")

  stack <- NULL
  expect_error(
    withCallingHandlers(
      error = function(...) stack <<- sys.calls(),
      summarise(mtcars, foobar())
    )
  )

  expect_true(some(stack, is_call, "foobar"))
})

test_that("summarise() gives meaningful errors", {
  eval(envir = global_env(), expr({
    expect_snapshot({
      # Messages about .groups=
      tibble(x = 1, y = 2) %>% group_by(x, y) %>% summarise()
      tibble(x = 1, y = 2) %>% rowwise(x, y) %>% summarise()
      tibble(x = 1, y = 2) %>% rowwise() %>% summarise()
    })
  }))

  eval(envir = global_env(), expr({
    expect_snapshot({
      # unsupported type
      (expect_error(
                      tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>%
                        summarise(a = rlang::env(a = 1))
      ))
      (expect_error(
                      tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>%
                        group_by(x, y) %>%
                        summarise(a = rlang::env(a = 1))
      ))
      (expect_error(
                      tibble(x = 1, y = c(1, 2, 2), z = runif(3)) %>%
                        rowwise() %>%
                        summarise(a = lm(y ~ x))
      ))

      # mixed types
      (expect_error(
                      tibble(id = 1:2, a = list(1, "2")) %>%
                        group_by(id) %>%
                        summarise(a = a[[1]])
      ))
      (expect_error(
                      tibble(id = 1:2, a = list(1, "2")) %>%
                        rowwise() %>%
                        summarise(a = a[[1]])
      ))

      # incompatible size
      (expect_error(
                      tibble(z = 1) %>%
                        summarise(x = 1:3, y = 1:2)
      ))
      (expect_error(
                      tibble(z = 1:2) %>%
                        group_by(z) %>%
                        summarise(x = 1:3, y = 1:2)
      ))
      (expect_error(
                      tibble(z = c(1, 3)) %>%
                        group_by(z) %>%
                        summarise(x = seq_len(z), y = 1:2)
      ))

      # mixed nulls
      (expect_error(
                      data.frame(x = 1:2, g = 1:2) %>% group_by(g) %>% summarise(x = if(g == 1) 42)
      ))
      (expect_error(
                      data.frame(x = 1:2, g = 1:2) %>% group_by(g) %>% summarise(x = if(g == 2) 42)
      ))

      # Missing variable
      (expect_error(summarise(mtcars, a = mean(not_there))))
      (expect_error(summarise(group_by(mtcars, cyl), a = mean(not_there))))

      # .data pronoun
      (expect_error(summarise(tibble(a = 1), c = .data$b)))
      (expect_error(summarise(group_by(tibble(a = 1:3), a), c = .data$b)))

      # Duplicate column names
      (expect_error(
                      tibble(x = 1, x = 1, .name_repair = "minimal") %>% summarise(x)
      ))

      # Not glue()ing
      (expect_error(tibble() %>% summarise(stop("{"))))
      (expect_error(
                      tibble(a = 1, b="{value:1, unit:a}") %>% group_by(b) %>% summarise(a = stop("!"))
      ))
    })
  }))

})

test_that("non-summary results are deprecated in favor of `reframe()` (#6382)", {
  local_options(lifecycle_verbosity = "warning")

  df <- tibble(g = c(1, 1, 2), x = 1:3)
  gdf <- group_by(df, g)
  rdf <- rowwise(df)

  expect_snapshot({
    out <- summarise(df, x = which(x < 3))
  })
  expect_identical(out$x, 1:2)

  expect_snapshot({
    out <- summarise(df, x = which(x < 3), .by = g)
  })
  expect_identical(out$g, c(1, 1))
  expect_identical(out$x, 1:2)

  # First group returns size 2 summary
  expect_snapshot({
    out <- summarise(gdf, x = which(x < 3))
  })
  expect_identical(out$g, c(1, 1))
  expect_identical(out$x, 1:2)

  # Last row returns size 0 summary
  expect_snapshot({
    out <- summarise(rdf, x = which(x < 3))
  })
  expect_identical(out$x, c(1L, 1L))
})
