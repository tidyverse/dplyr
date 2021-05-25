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
  # duplicated column name
  expect_snapshot(error = TRUE,
    tibble(x = 1, x = 1, .name_repair = "minimal") %>% arrange(x)
  )

  # error in mutate() step
  expect_snapshot(error = TRUE,
    tibble(x = 1) %>% arrange(y)
  )
  expect_snapshot(error = TRUE,
    tibble(x = 1) %>% arrange(rep(x, 2))
  )
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
  on.exit(removeClass("TestS4"))

  df <- tibble(x = 1:3, y = TestS4(3:1))
  expect_equal(arrange(df, y), df[3:1, ])
})

test_that("arrange defaults to the American English locale if stringi is installed", {
  skip_if_not_installed("stringi", "1.5.3")

  x <- c("A", "a", "b", "B")
  df <- tibble(x = x)

  res <- arrange(df, x)
  expect_identical(res$x, c("a", "A", "b", "B"))

  res <- arrange(df, desc(x))
  expect_identical(res$x, rev(c("a", "A", "b", "B")))
})

test_that("arrange falls back to the C locale with a warning if stringi is not available", {
  expect_snapshot(
    (expect_warning(locale_to_chr_transform(NULL, has_stringi = FALSE)))
  )
})

test_that("locale can be set to C", {
  x <- c("A", "a", "b", "B")
  df <- tibble(x = x)

  res <- arrange(df, x, .locale = "C")
  expect_identical(res$x, c("A", "B", "a", "b"))
})

test_that("non-English locales can be used", {
  skip_if_not_installed("stringi", "1.5.3")

  x <- c("casa", "\u00e1rbol", "zona", "\u00f3rgano")
  df <- tibble(x = x)

  res <- arrange(df, x, .locale = "es_MX")
  expect_identical(res$x, x[c(2, 1, 4, 3)])
})

test_that("arrange validates `.locale`", {
  df <- tibble()
  expect_snapshot({
    (expect_error(arrange(df, .locale = 1)))
    (expect_error(arrange(df, .locale = c("en_US", "fr_BF"))))
    (expect_error(arrange(df, .locale = "x")))
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

test_that("arrange() supports across() (#4679)", {
  df <- tibble(x = c(1, 3, 2, 1), y = c(4, 3, 2, 1))
  expect_identical(
    df %>% arrange(across()),
    df %>% arrange(x, y)
  )
  expect_identical(
    df %>% arrange(across(.fns = desc)),
    df %>% arrange(desc(x), desc(y))
  )
  expect_identical(
    df %>% arrange(across(x)),
    df %>% arrange(x)
  )
  expect_identical(
    df %>% arrange(across(y)),
    df %>% arrange(y)
  )
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
