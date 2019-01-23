context("Mutate - windowed")

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

test_that("cum(sum,min,max) works", {
  df <- data.frame(x = 1:10, y = seq(1, 10, by = 1), g = rep(c(1, 2), each = 5))

  res <- mutate(df,
    csumx = cumsum(x), csumy = cumsum(y),
    cminx = cummin(x), cminy = cummin(y),
    cmaxx = cummax(x), cmaxy = cummax(y)
  )
  expect_equal(res$csumx, cumsum(df$x))
  expect_equal(res$csumy, cumsum(df$y))
  expect_equal(res$cminx, cummin(df$x))
  expect_equal(res$cminy, cummin(df$y))
  expect_equal(res$cmaxx, cummax(df$x))
  expect_equal(res$cmaxy, cummax(df$y))

  res <- mutate(group_by(df, g),
    csumx = cumsum(x), csumy = cumsum(y),
    cminx = cummin(x), cminy = cummin(y),
    cmaxx = cummax(x), cmaxy = cummax(y)
  )
  expect_equal(res$csumx, c(cumsum(df$x[1:5]), cumsum(df$x[6:10])))
  expect_equal(res$csumy, c(cumsum(df$y[1:5]), cumsum(df$y[6:10])))
  expect_equal(res$cminx, c(cummin(df$x[1:5]), cummin(df$x[6:10])))
  expect_equal(res$cminy, c(cummin(df$y[1:5]), cummin(df$y[6:10])))
  expect_equal(res$cmaxx, c(cummax(df$x[1:5]), cummax(df$x[6:10])))
  expect_equal(res$cmaxy, c(cummax(df$y[1:5]), cummax(df$y[6:10])))

  df$x[3] <- NA
  df$y[4] <- NA
  res <- mutate(df,
    csumx = cumsum(x), csumy = cumsum(y),
    cminx = cummin(x), cminy = cummin(y),
    cmaxx = cummax(x), cmaxy = cummax(y)
  )
  expect_true(all(is.na(res$csumx[3:10])))
  expect_true(all(is.na(res$csumy[4:10])))

  expect_true(all(is.na(res$cminx[3:10])))
  expect_true(all(is.na(res$cminy[4:10])))

  expect_true(all(is.na(res$cmaxx[3:10])))
  expect_true(all(is.na(res$cmaxy[4:10])))
})

test_that("lead and lag simple hybrid version gives correct results (#133)", {
  res <- group_by(mtcars, cyl) %>%
    mutate(disp_lag_2 = lag(disp, 2), disp_lead_2 = lead(disp, 2)) %>%
    summarise(
      lag1 = all(is.na(head(disp_lag_2, 2))),
      lag2 = all(!is.na(tail(disp_lag_2, -2))),

      lead1 = all(is.na(tail(disp_lead_2, 2))),
      lead2 = all(!is.na(head(disp_lead_2, -2)))
    )

  expect_true(all(res$lag1))
  expect_true(all(res$lag2))

  expect_true(all(res$lead1))
  expect_true(all(res$lead2))
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

test_that("lag and lead work on factors inside mutate (#955)", {
  test_factor <- factor(rep(c("A", "B", "C"), each = 3))
  exp_lag  <- test_factor != lag(test_factor)
  exp_lead <- test_factor != lead(test_factor)

  test_df <- tbl_df(data.frame(test = test_factor))
  res <- test_df %>% mutate(
    is_diff_lag  = (test != lag(test)),
    is_diff_lead = (test != lead(test))
  )
  expect_equal(exp_lag, res$is_diff_lag)
  expect_equal(exp_lead, res$is_diff_lead)
})

test_that("lag handles default argument in mutate (#915)", {
  blah <- data.frame(x1 = c(5, 10, 20, 27, 35, 58, 5, 6), y = 8:1)
  blah <- mutate(blah,
    x2 = x1 - lag(x1, n = 1, default = 0),
    x3 = x1 - lead(x1, n = 1, default = 0),
    x4 = lag(x1, n = 1L, order_by = y),
    x5 = lead(x1, n = 1L, order_by = y)
  )
  expect_equal(blah$x2, blah$x1 - lag(blah$x1, n = 1, default = 0))
  expect_equal(blah$x3, blah$x1 - lead(blah$x1, n = 1, default = 0))
  expect_equal(blah$x4, lag(blah$x1, n = 1L, order_by = blah$y))
  expect_equal(blah$x5, lead(blah$x1, n = 1L, order_by = blah$y))
})

# FIXME: this should only fail if strict checking is on.
# test_that("window functions fail if db doesn't support windowing", {
#   df_sqlite <- temp_load(temp_srcs("sqlite"), df)$sql %>% group_by(g)
#   ok <- collect(df_sqlite %>% mutate(x > 4))
#   expect_equal(nrow(ok), 10)
#
#   expect_error(df_sqlite %>% mutate(x > mean(x)), "does not support")
#   expect_error(df_sqlite %>% mutate(r = row_number()), "does not support")
# })

test_that("dim attribute is stripped from grouped mutate (#1918)", {
  df <- data.frame(a = 1:3, b = 1:3)

  df_regular <- mutate(df, b = scale(b))
  df_grouped <- mutate(group_by(df, a), b = scale(b))
  df_rowwise <- mutate(rowwise(df), b = scale(b))

  expect_null(dim(df$b))
  expect_null(dim(df_grouped$b))
  expect_null(dim(df_rowwise$b))
})
