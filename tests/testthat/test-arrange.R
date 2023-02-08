# To turn on warnings from tibble::`names<-()`
local_options(lifecycle_verbosity = "warning")

test_that("empty arrange() returns input", {
  df <- tibble(x = 1:10, y = 1:10)
  gf <- group_by(df, x)

  expect_identical(arrange(df), df)
  expect_identical(arrange(gf), gf)

  expect_identical(arrange(df, !!!list()), df)
  expect_identical(arrange(gf, !!!list()), gf)
})

test_that("can sort empty data frame", {
  df <- tibble(a = numeric(0))
  expect_equal(arrange(df, a), df)
})

test_that("local arrange sorts missing values to end", {
  df <- data.frame(x = c(2, 1, NA))

  expect_equal(df %>% arrange(x) %>% pull(), c(1, 2, NA))
  expect_equal(df %>% arrange(desc(x)) %>% pull(), c(2, 1, NA))
})

test_that("arrange() gives meaningful errors", {
  expect_snapshot({
    # duplicated column name
    (expect_error(
      tibble(x = 1, x = 1, .name_repair = "minimal") %>% arrange(x)
    ))

    # error in mutate() step
    (expect_error(
      tibble(x = 1) %>% arrange(y)
    ))
    (expect_error(
      tibble(x = 1) %>% arrange(rep(x, 2))
    ))
  })

})

# column types ----------------------------------------------------------

test_that("arrange handles list columns (#282)", {
  # no intrinsic ordering
  df <- tibble(x = 1:3, y = list(3, 2, 1))
  expect_equal(arrange(df, y), df)

  df <- tibble(x = 1:3, y = list(sum, mean, sd))
  expect_equal(arrange(df, y), df)
})

test_that("arrange handles raw columns (#1803)", {
  df <- tibble(x = 1:3, y = as.raw(3:1))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange handles matrix columns", {
  df <- tibble(x = 1:3, y = matrix(6:1, ncol = 2))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange handles data.frame columns (#3153)", {
  df <- tibble(x = 1:3, y = data.frame(z = 3:1))
  expect_equal(arrange(df, y), tibble(x = 3:1, y = data.frame(z = 1:3)))
})

test_that("arrange handles complex columns", {
  df <- tibble(x = 1:3, y = 3:1 + 2i)
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange handles S4 classes (#1105)", {
  TestS4 <- suppressWarnings(setClass("TestS4", contains = "integer"))
  setMethod('[', 'TestS4', function(x, i, ...){ TestS4(unclass(x)[i, ...])  })
  on.exit(removeClass("TestS4"), add = TRUE)

  df <- tibble(x = 1:3, y = TestS4(3:1))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange works with two columns when the first has a data frame proxy (#6268)", {
  # `id1` has a data frame proxy for `vec_proxy_order()`
  df <- tibble(
    id1 = new_rcrd(list(x = 1, y = 1)),
    id2 = c(1, 3, 2)
  )

  out <- arrange(df, id1, id2)

  expect_identical(out$id2, c(1, 2, 3))
})

test_that("arrange ignores NULLs (#6193)", {
  df <- tibble(x = 1:2)
  y <- NULL

  out <- arrange(df, y, desc(x))
  expect_equal(out$x, 2:1)

  out <- arrange(df, y, desc(x), y)
  expect_equal(out$x, 2:1)
})

test_that("`arrange()` works with `numeric_version` (#6680)", {
  x <- numeric_version(c("1.11", "1.2.3", "1.2.2"))
  df <- tibble(x = x)

  expect <- df[c(3, 2, 1),]

  expect_identical(arrange(df, x), expect)
})

# locale --------------------------------------------------------------

test_that("arrange defaults to the C locale", {
  x <- c("A", "a", "b", "B")
  df <- tibble(x = x)

  res <- arrange(df, x)
  expect_identical(res$x, c("A", "B", "a", "b"))

  res <- arrange(df, desc(x))
  expect_identical(res$x, rev(c("A", "B", "a", "b")))
})

test_that("locale can be set to an English locale", {
  skip_if_not_installed("stringi", "1.5.3")

  x <- c("A", "a", "b", "B")
  df <- tibble(x = x)

  res <- arrange(df, x, .locale = "en")
  expect_identical(res$x, c("a", "A", "b", "B"))
})

test_that("non-English locales can be used", {
  skip_if_not_installed("stringi", "1.5.3")

  # Danish `o` with `/` through it sorts after `z` in Danish locale
  x <- c("o", "\u00F8", "p", "z")
  df <- tibble(x = x)

  # American English locale puts it right after `o`
  res <- arrange(df, x, .locale = "en")
  expect_identical(res$x, x)

  res <- arrange(df, x, .locale = "da")
  expect_identical(res$x, x[c(1, 3, 4, 2)])
})

test_that("arrange errors if stringi is not installed and a locale identifier is used", {
  expect_snapshot(error = TRUE, {
    locale_to_chr_proxy_collate("fr", has_stringi = FALSE)
  })
})

test_that("arrange validates `.locale`", {
  df <- tibble()

  expect_snapshot(error = TRUE, {
    arrange(df, .locale = 1)
  })
  expect_snapshot(error = TRUE, {
    arrange(df, .locale = c("en_US", "fr_BF"))
  })
})

test_that("arrange validates that `.locale` must be one from stringi", {
  skip_if_not_installed("stringi", "1.5.3")

  df <- tibble()

  expect_snapshot(error = TRUE, {
    arrange(df, .locale = "x")
  })
})

# data ----------------------------------------------------------------

test_that("arrange preserves input class", {
  df1 <- data.frame(x = 1:3, y = 3:1)
  df2 <- tibble(x = 1:3, y = 3:1)
  df3 <- df1 %>% group_by(x)

  expect_s3_class(arrange(df1, x), "data.frame", exact = TRUE)
  expect_s3_class(arrange(df2, x), "tbl_df")
  expect_s3_class(arrange(df3, x), "grouped_df")
})

test_that("grouped arrange ignores group, unless requested with .by_group", {
  df <- data.frame(g = c(2, 1, 2, 1), x = 4:1)
  gf <- group_by(df, g)

  expect_equal(arrange(gf, x), gf[4:1, ,])
  expect_equal(arrange(gf, x, .by_group = TRUE), gf[c(4, 2, 3, 1), ,])
})

test_that("arrange updates the grouping structure (#605)", {
  df <- tibble(g = c(2, 2, 1, 1), x = c(1, 3, 2, 4))
  res <- df %>% group_by(g) %>% arrange(x)
  expect_s3_class(res, "grouped_df")
  expect_equal(group_rows(res), list_of(c(2L, 4L), c(1L, 3L)))
})

test_that("arrange() supports across() and pick() (#4679)", {
  df <- tibble(x = c(1, 3, 2, 1), y = c(4, 3, 2, 1))

  expect_identical(
    df %>% arrange(pick(everything())),
    df %>% arrange(x, y)
  )
  expect_identical(
    df %>% arrange(across(everything(), .fns = desc)),
    df %>% arrange(desc(x), desc(y))
  )
  expect_identical(
    df %>% arrange(pick(x)),
    df %>% arrange(x)
  )
  expect_identical(
    df %>% arrange(across(y, .fns = identity)),
    df %>% arrange(y)
  )
})

test_that("arrange() works with across() and pick() cols that return multiple columns (#6490)", {
  df <- tibble(
    a = c(1, 1, 1),
    b = c(2, 2, 2),
    c = c(4, 4, 3),
    d = c(5, 2, 7)
  )

  expect_identical(
    arrange(df, across(c(a, b), .fns = identity), across(c(c, d), .fns = identity)),
    df[c(3, 2, 1),]
  )
  expect_identical(
    arrange(df, pick(a, b), pick(c, d)),
    df[c(3, 2, 1),]
  )
})

test_that("arrange() evaluates each pick() call on the original data (#6495)", {
  df <- tibble(x = 2:1)

  out <- arrange(df, TRUE, pick(everything()))
  expect_identical(out, df[c(2, 1),])

  out <- arrange(df, NULL, pick(everything()))
  expect_identical(out, df[c(2, 1),])
})

test_that("arrange() with empty dots still calls dplyr_row_slice()", {
  tbl <- new_tibble(list(x = 1), nrow = 1L)
  foo <- structure(tbl, class = c("foo_df", class(tbl)))

  local_methods(
    # `foo_df` always loses class when row slicing
    dplyr_row_slice.foo_df = function(data, i, ...) {
      out <- NextMethod()
      new_tibble(out, nrow = nrow(out))
    }
  )

  expect_s3_class(arrange(foo), class(tbl), exact = TRUE)
  expect_s3_class(arrange(foo, x), class(tbl), exact = TRUE)
})

test_that("can arrange() with unruly class", {
  local_methods(
    `[.dplyr_foobar` = function(x, i, ...) new_dispatched_quux(vec_slice(x, i)),
    dplyr_row_slice.dplyr_foobar = function(x, i, ...) x[i, ]
  )

  df <- foobar(data.frame(x = 1:3))
  expect_identical(
    arrange(df, desc(x)),
    quux(data.frame(x = 3:1, dispatched = TRUE))
  )
})

test_that("arrange() preserves the call stack on error (#5308)", {
  foobar <- function() stop("foo")

  stack <- NULL
  expect_error(
    withCallingHandlers(
      error = function(...) stack <<- sys.calls(),
      arrange(mtcars, foobar())
    )
  )

  expect_true(some(stack, is_call, "foobar"))
})

test_that("desc() inside arrange() checks the number of arguments (#5921)", {
  expect_snapshot({
    df <- data.frame(x = 1, y = 2)

    (expect_error(arrange(df, desc(x, y))))
  })
})

test_that("arrange keeps zero length groups",{
  df <- tibble(
    e = 1,
    f = factor(c(1, 1, 2, 2), levels = 1:3),
    g = c(1, 1, 2, 2),
    x = c(1, 2, 1, 4)
  )
  df <- group_by(df, e, f, g, .drop = FALSE)

  expect_equal( group_size(arrange(df)), c(2, 2, 0) )
  expect_equal( group_size(arrange(df, x)), c(2, 2, 0) )
})

# legacy --------------------------------------------------------------

test_that("legacy - using the global option `dplyr.legacy_locale` forces the system locale", {
  skip_if_not(has_collate_locale("en_US"), message = "Can't use 'en_US' locale")

  local_options(dplyr.legacy_locale = TRUE)
  withr::local_collate("en_US")

  df <- tibble(x = c("a", "A", "Z", "b"))

  expect_identical(arrange(df, x)$x, c("a", "A", "b", "Z"))
})

test_that("legacy - usage of `.locale` overrides `dplyr.legacy_locale`", {
  skip_if_not_installed("stringi", "1.5.3")

  local_options(dplyr.legacy_locale = TRUE)

  # Danish `o` with `/` through it sorts after `z` in Danish locale
  x <- c("o", "\u00F8", "p", "z")
  df <- tibble(x = x)

  # American English locale puts it right after `o`
  res <- arrange(df, x, .locale = "en")
  expect_identical(res$x, x)

  res <- arrange(df, x, .locale = "da")
  expect_identical(res$x, x[c(1, 3, 4, 2)])
})

test_that("legacy - empty arrange() returns input", {
  local_options(dplyr.legacy_locale = TRUE)

  df <- tibble(x = 1:10, y = 1:10)
  gf <- group_by(df, x)

  expect_identical(arrange(df), df)
  expect_identical(arrange(gf), gf)

  expect_identical(arrange(df, !!!list()), df)
  expect_identical(arrange(gf, !!!list()), gf)
})

test_that("legacy - can sort empty data frame", {
  local_options(dplyr.legacy_locale = TRUE)

  df <- tibble(a = numeric(0))
  expect_equal(arrange(df, a), df)
})

test_that("legacy - local arrange sorts missing values to end", {
  local_options(dplyr.legacy_locale = TRUE)

  df <- data.frame(x = c(2, 1, NA))

  expect_equal(df %>% arrange(x) %>% pull(), c(1, 2, NA))
  expect_equal(df %>% arrange(desc(x)) %>% pull(), c(2, 1, NA))
})

test_that("legacy - arrange handles list columns (#282)", {
  local_options(dplyr.legacy_locale = TRUE)

  # no intrinsic ordering
  df <- tibble(x = 1:3, y = list(3, 2, 1))
  expect_equal(arrange(df, y), df)

  df <- tibble(x = 1:3, y = list(sum, mean, sd))
  expect_equal(arrange(df, y), df)
})

test_that("legacy - arrange handles raw columns (#1803)", {
  local_options(dplyr.legacy_locale = TRUE)

  df <- tibble(x = 1:3, y = as.raw(3:1))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("legacy - arrange handles matrix columns", {
  local_options(dplyr.legacy_locale = TRUE)

  df <- tibble(x = 1:3, y = matrix(6:1, ncol = 2))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("legacy - arrange handles data.frame columns (#3153)", {
  local_options(dplyr.legacy_locale = TRUE)

  df <- tibble(x = 1:3, y = data.frame(z = 3:1))
  expect_equal(arrange(df, y), tibble(x = 3:1, y = data.frame(z = 1:3)))
})

test_that("legacy - arrange handles complex columns", {
  local_options(dplyr.legacy_locale = TRUE)

  df <- tibble(x = 1:3, y = 3:1 + 2i)
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("legacy - arrange handles S4 classes (#1105)", {
  local_options(dplyr.legacy_locale = TRUE)

  TestS4 <- suppressWarnings(setClass("TestS4", contains = "integer"))
  setMethod('[', 'TestS4', function(x, i, ...){ TestS4(unclass(x)[i, ...])  })
  on.exit(removeClass("TestS4"), add = TRUE)

  df <- tibble(x = 1:3, y = TestS4(3:1))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("legacy - `arrange()` works with `numeric_version` (#6680)", {
  local_options(dplyr.legacy_locale = TRUE)

  x <- numeric_version(c("1.11", "1.2.3", "1.2.2"))
  df <- tibble(x = x)

  expect <- df[c(3, 2, 1),]

  expect_identical(arrange(df, x), expect)
})

test_that("legacy - arrange works with two columns when the first has a data frame proxy (#6268)", {
  local_options(dplyr.legacy_locale = TRUE)

  # `id1` has a data frame proxy for `vec_proxy_order()`
  df <- tibble(
    id1 = new_rcrd(list(x = 1, y = 1)),
    id2 = c(1, 3, 2)
  )

  out <- arrange(df, id1, id2)

  expect_identical(out$id2, c(1, 2, 3))
})

test_that("legacy - arrange() supports across() and pick() (#4679)", {
  local_options(dplyr.legacy_locale = TRUE)

  df <- tibble(x = c(1, 3, 2, 1), y = c(4, 3, 2, 1))

  expect_identical(
    df %>% arrange(pick(everything())),
    df %>% arrange(x, y)
  )
  expect_identical(
    df %>% arrange(across(everything(), .fns = desc)),
    df %>% arrange(desc(x), desc(y))
  )
  expect_identical(
    df %>% arrange(pick(x)),
    df %>% arrange(x)
  )
  expect_identical(
    df %>% arrange(across(y, .fns = identity)),
    df %>% arrange(y)
  )
})

test_that("legacy - arrange() works with across() and pick() cols that return multiple columns (#6490)", {
  local_options(dplyr.legacy_locale = TRUE)

  df <- tibble(
    a = c(1, 1, 1),
    b = c(2, 2, 2),
    c = c(4, 4, 3),
    d = c(5, 2, 7)
  )

  expect_identical(
    arrange(df, across(c(a, b), .fns = identity), across(c(c, d), .fns = identity)),
    df[c(3, 2, 1),]
  )
  expect_identical(
    arrange(df, pick(a, b), pick(c, d)),
    df[c(3, 2, 1),]
  )
})

test_that("legacy - arrange sorts missings in df-cols correctly", {
  local_options(dplyr.legacy_locale = TRUE)

  col <- tibble(a = c(1, 1, 1), b = c(3, NA, 1))
  df <- tibble(x = col)

  expect_identical(arrange(df, x), df[c(3, 1, 2),])
  expect_identical(arrange(df, desc(x)), df[c(1, 3, 2),])
})

test_that("legacy - arrange with duplicates in a df-col uses a stable sort", {
  local_options(dplyr.legacy_locale = TRUE)

  col <- tibble(a = c(1, 1, 1, 1, 1), b = c(3, NA, 2, 3, NA))
  df <- tibble(x = col, y = 1:5)

  expect_identical(arrange(df, x)$y, c(3L, 1L, 4L, 2L, 5L))
  expect_identical(arrange(df, desc(x))$y, c(1L, 4L, 3L, 2L, 5L))
})

test_that("legacy - arrange with doubly nested df-col doesn't infloop", {
  local_options(dplyr.legacy_locale = TRUE)

  one <- tibble(a = c(1, 1, 1, 1, 1), b = c(1, 1, 2, 2, 2))
  two <- tibble(a = c(1, 1, 1, 1, 1), b = c(2, 1, 1, 2, 2))
  col <- tibble(one = one, two = two)
  df <- tibble(x = col, y = c(1, 1, 1, 1, 0))

  expect_identical(arrange(df, x, y), df[c(2, 1, 3, 5, 4),])
})
