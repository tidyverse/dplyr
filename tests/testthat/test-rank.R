ntile_h <- function(x, n) {
  tibble(x = x) %>%
    mutate(y = ntile(x, n)) %>%
    pull(y)
}

ntile_h_dplyr <- function(x, n) {
  tibble(x = x) %>%
    mutate(y = dplyr::ntile(x, n)) %>%
    pull(y)
}
test_that("ntile ignores number of NAs", {
  x <- c(1:3, NA, NA, NA)

  expect_equal(ntile(x, 3), x)
  expect_equal(ntile_h(x, 3), x)

  x1 <- c(1L, 1L, 1L, NA, NA, NA)
  expect_equal(ntile(x, 1), x1)
  expect_equal(ntile_h(x, 1), x1)
})

test_that("ntile always returns an integer", {
  expect_equal(ntile(numeric(), 3), integer())
  expect_equal(ntile_h(numeric(), 3), integer())

  expect_equal(ntile(NA, 3), NA_integer_)
  expect_equal(ntile_h(NA, 3), NA_integer_)
})

test_that("ntile handles character vectors consistently", {
  charvec_sort_test <- function() {
    x1 <- c("[", "]", NA, "B", "y", "a", "Z")
    x2 <- c("a", "b", "C")

    expect_equal(ntile_h(x1, 3), ntile_h_dplyr(x1, 3))
    expect_equal(ntile_h(x2, 2), ntile_h_dplyr(x2, 2))
  }

  # Test against both the local, and the C locale for collation
  charvec_sort_test()
  withr::with_collate("C", charvec_sort_test())
})

test_that("ntile() does not overflow (#4186)", {
  res <- tibble(a = 1:1e5) %>%
    mutate(b = ntile(n = 1e5)) %>%
    count(b) %>%
    pull()

  expect_true(all(res == 1L))
})


test_that("ntile works with one argument (#3418)", {
  df <- data.frame(x=1:42)
  expect_identical(
    mutate( df, nt = ntile(n = 9)),
    mutate( df, nt = ntile(row_number(), n = 9))
  )

  df <- group_by( data.frame(x=1:42, g = rep(1:7, each=6)), g )
  expect_identical(
    mutate( df, nt = ntile(n = 4)),
    mutate( df, nt = ntile(row_number(), n = 4))
  )
})


test_that("rank functions deal correctly with NA (#774)", {
  data <- tibble(x = c(1, 2, NA, 1, 0, NA))
  res <- data %>% mutate(
    min_rank = min_rank(x),
    percent_rank = percent_rank(x),
    dense_rank = dense_rank(x),
    cume_dist = cume_dist(x),
    ntile = ntile(x, 2),
    row_number = row_number(x)
  )
  expect_true(all(is.na(res$min_rank[c(3, 6)])))
  expect_true(all(is.na(res$dense_rank[c(3, 6)])))
  expect_true(all(is.na(res$percent_rank[c(3, 6)])))
  expect_true(all(is.na(res$cume_dist[c(3, 6)])))
  expect_true(all(is.na(res$ntile[c(3, 6)])))
  expect_true(all(is.na(res$row_number[c(3, 6)])))

  expect_equal(res$percent_rank[ c(1, 2, 4, 5) ], c(1 / 3, 1, 1 / 3, 0))
  expect_equal(res$min_rank[ c(1, 2, 4, 5) ], c(2L, 4L, 2L, 1L))
  expect_equal(res$dense_rank[ c(1, 2, 4, 5) ], c(2L, 3L, 2L, 1L))
  expect_equal(res$cume_dist[ c(1, 2, 4, 5) ], c(.75, 1, .75, .25))
  expect_equal(res$ntile[ c(1, 2, 4, 5) ], c(1L, 2L, 2L, 1L))
  expect_equal(res$row_number[ c(1, 2, 4, 5) ], c(2L, 4L, 3L, 1L))

  data <- tibble(
    x = rep(c(1, 2, NA, 1, 0, NA), 2),
    g = rep(c(1, 2), each = 6)
  )
  res <- data %>%
    group_by(g) %>%
    mutate(
      min_rank = min_rank(x),
      percent_rank = percent_rank(x),
      dense_rank = dense_rank(x),
      cume_dist = cume_dist(x),
      ntile = ntile(x, 2),
      row_number = row_number(x)
    )
  expect_true(all(is.na(res$min_rank[c(3, 6, 9, 12)])))
  expect_true(all(is.na(res$dense_rank[c(3, 6, 9, 12)])))
  expect_true(all(is.na(res$percent_rank[c(3, 6, 9, 12)])))
  expect_true(all(is.na(res$cume_dist[c(3, 6, 9, 12)])))
  expect_true(all(is.na(res$ntile[c(3, 6, 9, 12)])))
  expect_true(all(is.na(res$row_number[c(3, 6, 9, 12)])))

  expect_equal(res$percent_rank[ c(1, 2, 4, 5, 7, 8, 10, 11) ], rep(c(1 / 3, 1, 1 / 3, 0), 2))
  expect_equal(res$min_rank[ c(1, 2, 4, 5, 7, 8, 10, 11) ], rep(c(2L, 4L, 2L, 1L), 2))
  expect_equal(res$dense_rank[ c(1, 2, 4, 5, 7, 8, 10, 11) ], rep(c(2L, 3L, 2L, 1L), 2))
  expect_equal(res$cume_dist[ c(1, 2, 4, 5, 7, 8, 10, 11) ], rep(c(.75, 1, .75, .25), 2))
  expect_equal(res$ntile[ c(1, 2, 4, 5, 7, 8, 10, 11) ], rep(c(1L, 2L, 2L, 1L), 2))
  expect_equal(res$row_number[ c(1, 2, 4, 5, 7, 8, 10, 11) ], rep(c(2L, 4L, 3L, 1L), 2))
})



test_that("row_number handles empty data frames (#762)", {
  df <- data.frame(a = numeric(0))
  res <- df %>% mutate(
    row_number_0 = row_number(),
    row_number_a = row_number(a),
    ntile = ntile(a, 2),
    min_rank = min_rank(a),
    percent_rank = percent_rank(a),
    dense_rank = dense_rank(a),
    cume_dist = cume_dist(a)
  )
  expect_equal(
    names(res),
    c("a", "row_number_0", "row_number_a", "ntile", "min_rank", "percent_rank", "dense_rank", "cume_dist")
  )
  expect_equal(nrow(res), 0L)
})


test_that("row_number gives correct results", {
  tmp <- data.frame(
    id = rep(c(1, 2), each = 5), value = c(1, 1, 2, 5, 0, 6, 4, 0, 0, 2),
    s = c(letters[1:2], LETTERS[1:4], letters[2:5])
  )

  res <- group_by(tmp, id) %>% mutate(var = row_number(value))
  expect_equal(res$var, c(2, 3, 4, 5, 1, 5, 4, 1, 2, 3))

  # Test character vector sorting by comparing C and R function outputs
  # Should be careful of testing against static return values due to locale differences
  charvec_sort_test <- function(tmp) {
    res2 <- group_by(tmp, id) %>% mutate(var = row_number(s), var_d = dplyr::row_number(s))
    expect_equal(res2$var, res2$var_d)

    res3 <- data.frame(s = c("[", "]", NA, "a", "Z")) %>% mutate(var = row_number(s), var_d = dplyr::row_number(s))
    expect_equal(res3$var, res3$var_d)
  }

  # Test against both the local, and the C locale for collation
  charvec_sort_test(tmp)
  withr::with_collate("C", charvec_sort_test(tmp))
})

test_that("row_number works with 0 arguments", {
  g <- group_by(mtcars, cyl)
  expect_equal(mutate(g, rn = row_number()), mutate(g, rn = 1:n()))
})



test_that("lead/lag inside mutate handles expressions as value for default (#1411) ", {
  df <- tibble(x = 1:3)
  res <- mutate(df, leadn = lead(x, default = x[1]), lagn = lag(x, default = x[1]))
  expect_equal(res$leadn, lead(df$x, default = df$x[1]))
  expect_equal(res$lagn, lag(df$x, default = df$x[1]))

  res <- mutate(df, leadn = lead(x, default = c(1)), lagn = lag(x, default = c(1)))
  expect_equal(res$leadn, lead(df$x, default = 1))
  expect_equal(res$lagn, lag(df$x, default = 1))
})

test_that("ntile puts large groups first (#4995) ", {
  expect_equal(ntile(1, 5), 1)
  expect_equal(ntile(1:2, 5), 1:2)
  expect_equal(ntile(1:3, 5), 1:3)
  expect_equal(ntile(1:4, 5), 1:4)
  expect_equal(ntile(1:5, 5), 1:5)
  expect_equal(ntile(1:6, 5), c(1, 1:5))

  expect_equal(ntile(1, 7), 1)
  expect_equal(ntile(1:2, 7), 1:2)
  expect_equal(ntile(1:3, 7), 1:3)
  expect_equal(ntile(1:4, 7), 1:4)
  expect_equal(ntile(1:5, 7), 1:5)
  expect_equal(ntile(1:6, 7), 1:6)
  expect_equal(ntile(1:7, 7), 1:7)
  expect_equal(ntile(1:8, 7), c(1, 1:7))
})

test_that("desc is correctly handled by window functions", {
  df <- data.frame(
    x = 1:10, y = seq(1, 10, by = 1),
    g = rep(c(1, 2), each = 5), s = c(letters[1:3], LETTERS[1:5], letters[4:5])
  )

  expect_equal(mutate(df, rank = min_rank(desc(x)))$rank, 10:1)
  expect_equal(mutate(group_by(df, g), rank = min_rank(desc(x)))$rank, rep(5:1, 2))

  expect_equal(mutate(df, rank = row_number(desc(x)))$rank, 10:1)
  expect_equal(mutate(group_by(df, g), rank = row_number(desc(x)))$rank, rep(5:1, 2))

  # Test character vector sorting
  charvec_sort_test <- function(df) {
    expect_equal(
      mutate(df, rank = row_number(desc(s)))$rank,
      mutate(df, rank = dplyr::row_number(desc(s)))$rank
    )
    expect_equal(
      mutate(group_by(df, g), rank = row_number(desc(s)))$rank,
      mutate(group_by(df, g), rank = dplyr::row_number(desc(s)))$rank
    )
  }

  # Test against both the local, and the C locale for collation
  charvec_sort_test(df)
  withr::with_collate("C", charvec_sort_test(df))
})


test_that("min_rank handles columns full of NaN (#726)", {
  test <- data.frame(
    Name = c("a", "b", "c", "d", "e"),
    ID = c(1, 1, 1, 1, 1),
    expression = c(NaN, NaN, NaN, NaN, NaN)
  )
  data <- group_by(test, ID) %>% mutate(rank = min_rank(expression))
  expect_true(all(is.na(data$rank)))
})


test_that("percent_rank ignores NAs (#1132)", {
  expect_equal(percent_rank(c(1:3, NA)), c(0, 0.5, 1, NA))
})

test_that("cume_dist ignores NAs (#1132)", {
  expect_equal(cume_dist(c(1:3, NA)), c(1 / 3, 2 / 3, 1, NA))
})
