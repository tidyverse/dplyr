context("hybrid")

# test_that("hybrid evaluation environment is cleaned up (#2358)", {
#   # Can't use pipe here, f and g should have top-level parent.env()
#   df <- data_frame(a = 1)
#   df <- mutate(df, f = {
#     a
#     list(function() {})
#   })
#   df <- mutate(df, g = {
#     f
#     list(quo(.))
#   })
#   df <- mutate(df, h = {
#     g
#     list(~ .)
#   })
#   df <- mutate(df, i = {
#     h
#     list(.data)
#   })
#
#   expect_true(env_has(df$f[[1]], "a", inherit = TRUE))
#   expect_true(env_has(df$g[[1]], "f", inherit = TRUE))
#   expect_true(env_has(df$h[[1]], "g", inherit = TRUE))
#
#   expect_warning(
#     expect_null(env_get(df$f[[1]], "a", inherit = TRUE)),
#     "Hybrid callback proxy out of scope",
#     fixed = TRUE
#   )
#   expect_warning(
#     expect_null(env_get(df$g[[1]], "f", inherit = TRUE)),
#     "Hybrid callback proxy out of scope",
#     fixed = TRUE
#   )
#   expect_warning(
#     expect_null(env_get(df$h[[1]], "g", inherit = TRUE)),
#     "Hybrid callback proxy out of scope",
#     fixed = TRUE
#   )
#   expect_warning(
#     expect_null(df$i[[1]]$h),
#     "Hybrid callback proxy out of scope",
#     fixed = TRUE
#   )
# })
#
# test_that("n() and n_distinct() work", {
#   check_hybrid_result(
#     n(),
#     a = 1:5,
#     expected = 5L, test_eval = FALSE
#   )
#   check_not_hybrid_result(
#     list(1:n()),
#     a = 1:5,
#     expected = list(1:5), test_eval = FALSE
#   )
#
#   check_hybrid_result(
#     n_distinct(a),
#     a = 1:5,
#     expected = 5L
#   )
#   check_hybrid_result(
#     n_distinct(a),
#     a = rep(1L, 3),
#     expected = 1L
#   )
#   check_hybrid_result(
#     n_distinct(a, b),
#     a = rep(1L, 3), b = 1:3,
#     expected = 3L
#   )
#   check_hybrid_result(
#     n_distinct(a, b),
#     a = rep(1L, 3), b = c(1, 1, 2),
#     expected = 2L
#   )
#   check_hybrid_result(
#     n_distinct(a, b),
#     a = rep(1L, 3), b = c(1, 1, NA),
#     expected = 2L
#   )
#   check_hybrid_result(
#     n_distinct(a, b, na.rm = TRUE),
#     a = rep(1L, 3), b = c(1, 1, NA),
#     expected = 1L
#   )
#   check_hybrid_result(
#     n_distinct(a = a, b = b, na.rm = TRUE),
#     a = rep(1L, 3), b = c(1, 1, NA),
#     expected = 1L
#   )
#
#   expect_not_hybrid_error(
#     n_distinct(),
#     a = 1:5,
#     error = "Need at least one column for `n_distinct[(][)]`"
#   )
# })
#
# test_that("%in% works (#192)", {
#   # compilation errors on Windows
#   # https://ci.appveyor.com/project/hadley/dplyr/build/1.0.230
#   check_not_hybrid_result(
#     list(a %in% (1:3 * 1i)),
#     a = 2:4 * 1i,
#     expected = list(c(TRUE, TRUE, FALSE))
#   )
#
#   check_not_hybrid_result(
#     list(a %in% 1:3),
#     a = as.numeric(2:4),
#     expected = list(c(TRUE, TRUE, FALSE))
#   )
#   check_not_hybrid_result(
#     list(a %in% as.numeric(1:3)),
#     a = 2:4,
#     expected = list(c(TRUE, TRUE, FALSE))
#   )
#
#   c <- 2:4
#   check_not_hybrid_result(
#     list(c %in% 1:3),
#     a = as.numeric(2:4),
#     expected = list(c(TRUE, TRUE, FALSE))
#   )
# })
#
# test_that("min() and max() work", {
#   check_hybrid_result(
#     min(a),
#     a = 1:5,
#     expected = 1,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     max(a),
#     a = 1:5,
#     expected = 5,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     min(a),
#     a = as.numeric(1:5),
#     expected = 1
#   )
#   check_hybrid_result(
#     max(a),
#     a = as.numeric(1:5),
#     expected = 5
#   )
#   check_hybrid_result(
#     min(a),
#     a = c(1:5, NA),
#     expected = is.na,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     max(a),
#     a = c(1:5, NA),
#     expected = is.na,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     min(a),
#     a = c(NA, 1:5),
#     expected = is.na,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     max(a),
#     a = c(NA, 1:5),
#     expected = is.na,
#     test_eval = FALSE
#   )
#
#   c <- 1:3
#   check_not_hybrid_result(
#     min(c),
#     a = 1:5,
#     expected = 1L
#   )
#   check_not_hybrid_result(
#     max(c),
#     a = 1:5,
#     expected = 3L
#   )
#
#   check_not_hybrid_result(
#     min(a),
#     a = letters,
#     expected = "a"
#   )
#   check_not_hybrid_result(
#     max(a),
#     a = letters,
#     expected = "z"
#   )
#   check_not_hybrid_result(
#     min(a),
#     a = c(letters, NA),
#     expected = NA_character_
#   )
#   check_not_hybrid_result(
#     max(a),
#     a = c(letters, NA),
#     expected = NA_character_
#   )
#   check_not_hybrid_result(
#     min(a, na.rm = TRUE),
#     a = c(letters, NA),
#     expected = "a"
#   )
#   check_not_hybrid_result(
#     max(a, na.rm = TRUE),
#     a = c(letters, NA),
#     expected = "z"
#   )
#
#   check_hybrid_result(
#     min(a, na.rm = TRUE),
#     a = NA_real_,
#     expected = Inf,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     max(a, na.rm = TRUE),
#     a = NA_real_,
#     expected = -Inf,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     min(a),
#     a = numeric(),
#     expected = Inf,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     max(a),
#     a = numeric(),
#     expected = -Inf,
#     test_eval = FALSE
#   )
#
#   check_hybrid_result(
#     max(a, na.rm = TRUE),
#     a = NA_integer_,
#     expected = -Inf,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     min(a, na.rm = TRUE),
#     a = NA_integer_,
#     expected = Inf,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     max(a),
#     a = integer(),
#     expected = -Inf,
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     min(a),
#     a = integer(),
#     expected = Inf,
#     test_eval = FALSE
#   )
# })
#
# test_that("first(), last(), and nth() work", {
#   check_hybrid_result(
#     first(a),
#     a = 1:5,
#     expected = 1L
#   )
#   check_hybrid_result(
#     last(a),
#     a = as.numeric(1:5),
#     expected = 5
#   )
#   check_hybrid_result(
#     nth(a, 6, default = 3),
#     a = as.numeric(1:5),
#     expected = 3
#   )
#   check_hybrid_result(
#     nth(a, 6, def = 3),
#     a = as.numeric(1:5),
#     expected = 3
#   )
#   check_hybrid_result(
#     nth(a, 6.5),
#     a = 1:5,
#     expected = NA_integer_
#   )
#
#   check_not_hybrid_result(
#     nth(a, b[[2]]),
#     a = letters[1:5], b = 5:1,
#     expected = "d"
#   )
#
#   check_hybrid_result(
#     nth(a, 3),
#     a = as.numeric(1:5) * 1i,
#     expected = 3i
#   )
#   check_not_hybrid_result(
#     nth(a, 2),
#     a = as.list(1:5),
#     expected = 2L
#   )
#
#   check_not_hybrid_result(
#     nth(a, order_by = 5:1, 2),
#     a = 1:5,
#     expected = 4L
#   )
#   expect_not_hybrid_error(
#     first(a, bogus = 3),
#     a = 1:5,
#     error = "unused argument"
#   )
#   expect_not_hybrid_error(
#     last(a, bogus = 3),
#     a = 1:5,
#     error = "unused argument"
#   )
#   expect_not_hybrid_error(
#     nth(a, 3, bogus = 3),
#     a = 1:5,
#     error = "unused argument"
#   )
#
#   c <- 1:3
#   check_not_hybrid_result(
#     first(c),
#     a = 2:4,
#     expected = 1L
#   )
#   check_not_hybrid_result(
#     last(c),
#     a = 2:4,
#     expected = 3L
#   )
#   check_not_hybrid_result(
#     nth(c, 2),
#     a = 2:4,
#     expected = 2L
#   )
#
#   check_hybrid_result(
#     first(a, order_by = b),
#     a = 1:5, b = 5:1,
#     expected = 5L
#   )
#
#   default_value <- 6L
#   check_not_hybrid_result(
#     nth(a, 6, default = default_value),
#     a = 1:5,
#     expected = 6L
#   )
#
#   expect_equal(
#     tibble(a = c(1, 1, 2), b = letters[1:3]) %>%
#       group_by(a) %>%
#       summarize(b = b[1], b = first(b)) %>%
#       ungroup(),
#     tibble(a = c(1, 2), b = c("a", "c"))
#   )
# })
#
# test_that("lead() and lag() work", {
#   check_hybrid_result(
#     list(lead(a)),
#     a = 1:5,
#     expected = list(c(2:5, NA))
#   )
#   check_hybrid_result(
#     list(lag(a)),
#     a = 1:5,
#     expected = list(c(NA, 1:4))
#   )
#
#   check_hybrid_result(
#     list(lead(a)),
#     a = as.numeric(1:5),
#     expected = list(c(as.numeric(2:5), NA))
#   )
#   check_hybrid_result(
#     list(lag(a)),
#     a = as.numeric(1:5),
#     expected = list(c(NA, as.numeric(1:4)))
#   )
#
#   check_hybrid_result(
#     list(lead(a)),
#     a = letters[1:5],
#     expected = list(c(letters[2:5], NA))
#   )
#   check_hybrid_result(
#     list(lag(a)),
#     a = letters[1:5],
#     expected = list(c(NA, letters[1:4]))
#   )
#
#   check_hybrid_result(
#     list(lead(a)),
#     a = c(TRUE, FALSE),
#     expected = list(c(FALSE, NA))
#   )
#   check_hybrid_result(
#     list(lag(a)),
#     a = c(TRUE, FALSE),
#     expected = list(c(NA, TRUE))
#   )
#
#   check_not_hybrid_result(
#     list(lead(a, order_by = b)),
#     a = 1:5, b = 5:1,
#     expected = list(c(NA, 1:4))
#   )
#   check_not_hybrid_result(
#     list(lag(a, order_by = b)),
#     a = 1:5, b = 5:1,
#     expected = list(c(2:5, NA))
#   )
#
#   check_hybrid_result(
#     list(lead(a)),
#     a = 1:5 * 1i,
#     expected = list(c(2:5, NA) * 1i)
#   )
#   check_hybrid_result(
#     list(lag(a)),
#     a = 1:5 * 1i,
#     expected = list(c(NA, 1:4) * 1i)
#   )
#
#   v <- 1:2
#   check_not_hybrid_result(
#     list(lead(a, v[1])),
#     a = 1:5,
#     expected = list(c(2:5, NA))
#   )
#   check_not_hybrid_result(
#     list(lag(a, v[1])),
#     a = 1:5,
#     expected = list(c(NA, 1:4))
#   )
# })
#
# test_that("mean(), var(), sd() and sum() work", {
#   check_hybrid_result(
#     mean(a),
#     a = 1:5,
#     expected = 3
#   )
#   check_hybrid_result(
#     mean(a),
#     a = as.numeric(1:5),
#     expected = 3
#   )
#   check_hybrid_result(
#     var(a),
#     a = 1:3,
#     expected = 1
#   )
#   check_hybrid_result(
#     var(a),
#     a = as.numeric(1:3),
#     expected = 1
#   )
#   check_hybrid_result(
#     sd(a),
#     a = 1:3,
#     expected = 1
#   )
#   check_hybrid_result(
#     sd(a),
#     a = as.numeric(1:3),
#     expected = 1
#   )
#   check_hybrid_result(
#     sum(a),
#     a = 1:5,
#     expected = 15L
#   )
#   check_hybrid_result(
#     sum(a),
#     a = as.numeric(1:5),
#     expected = 15
#   )
#
#   check_hybrid_result(
#     mean(a),
#     a = c(1:5, NA),
#     expected = is.na
#   )
#   check_hybrid_result(
#     mean(a),
#     a = as.numeric(c(1:5, NA)),
#     expected = is.na
#   )
#   check_hybrid_result(
#     var(a),
#     a = c(1:3, NA),
#     expected = is.na
#   )
#   check_hybrid_result(
#     var(a),
#     a = as.numeric(c(1:3, NA)),
#     expected = is.na
#   )
#   check_hybrid_result(
#     sd(a),
#     a = c(1:3, NA),
#     expected = is.na
#   )
#   check_hybrid_result(
#     sd(a),
#     a = as.numeric(c(1:3, NA)),
#     expected = is.na
#   )
#   check_hybrid_result(
#     sum(a),
#     a = c(1:5, NA),
#     expected = NA_integer_
#   )
#   check_hybrid_result(
#     sum(a),
#     a = c(as.numeric(1:5), NA),
#     expected = is.na
#   )
#
#   check_hybrid_result(
#     mean(a, na.rm = TRUE),
#     a = c(1:5, NA),
#     expected = 3
#   )
#   check_hybrid_result(
#     mean(a, na.rm = TRUE),
#     a = as.numeric(c(1:5, NA)),
#     expected = 3
#   )
#   check_hybrid_result(
#     var(a, na.rm = TRUE),
#     a = c(1:3, NA),
#     expected = 1
#   )
#   check_hybrid_result(
#     var(a, na.rm = TRUE),
#     a = as.numeric(c(1:3, NA)),
#     expected = 1
#   )
#   check_hybrid_result(
#     sd(a, na.rm = TRUE),
#     a = c(1:3, NA),
#     expected = 1
#   )
#   check_hybrid_result(
#     sd(a, na.rm = TRUE),
#     a = as.numeric(c(1:3, NA)),
#     expected = 1
#   )
#   check_hybrid_result(
#     sum(a, na.rm = TRUE),
#     a = c(1:5, NA),
#     expected = 15L
#   )
#   check_hybrid_result(
#     sum(a, na.rm = TRUE),
#     a = c(as.numeric(1:5), NA),
#     expected = 15
#   )
#
#   check_not_hybrid_result(
#     sd(a, TRUE),
#     a = c(1:3, NA),
#     expected = 1
#   )
#
#   check_not_hybrid_result(
#     sd(a, na.rm = b[[1]]),
#     a = c(1:3, NA), b = TRUE,
#     expected = 1
#   )
#
#   check_hybrid_result(
#     sd(a),
#     a = c(1:3, NA),
#     expected = is.na
#   )
# })
#
# test_that("row_number(), ntile(), min_rank(), percent_rank(), dense_rank(), and cume_dist() work", {
#   check_hybrid_result(
#     list(row_number()),
#     a = 1:5,
#     expected = list(1:5),
#     test_eval = FALSE
#   )
#   check_hybrid_result(
#     list(row_number(a)),
#     a = 5:1,
#     expected = list(5:1)
#   )
#   check_hybrid_result(
#     list(min_rank(a)),
#     a = c(1, 3, 2, 3, 1),
#     expected = list(c(1L, 4L, 3L, 4L, 1L))
#   )
#   check_hybrid_result(
#     list(percent_rank(a)),
#     a = c(1, 3, 2, 3, 1),
#     expected = list((c(1L, 4L, 3L, 4L, 1L) - 1) / 4)
#   )
#   check_hybrid_result(
#     list(cume_dist(a)),
#     a = c(1, 3, 2, 3),
#     expected = list(c(0.25, 1.0, 0.5, 1.0))
#   )
#   check_hybrid_result(
#     list(dense_rank(a)),
#     a = c(1, 3, 2, 3, 1),
#     expected = list(c(1L, 3L, 2L, 3L, 1L))
#   )
#
#   expect_not_hybrid_error(
#     row_number(a, 1),
#     a = 5:1,
#     error = "unused argument"
#   )
#   expect_not_hybrid_error(
#     min_rank(a, 1),
#     a = 5:1,
#     error = "unused argument"
#   )
#   expect_not_hybrid_error(
#     percent_rank(a, 1),
#     a = 5:1,
#     error = "unused argument"
#   )
#   expect_not_hybrid_error(
#     cume_dist(a, 1),
#     a = 5:1,
#     error = "unused argument"
#   )
#   expect_not_hybrid_error(
#     dense_rank(a, 1),
#     a = 5:1,
#     error = "unused argument"
#   )
#   expect_not_hybrid_error(
#     ntile(a, 2, 1),
#     a = 5:1,
#     error = "unused argument"
#   )
#
#   check_not_hybrid_result(
#     row_number("a"),
#     a = 5:1,
#     expected = 1L
#   )
#   check_not_hybrid_result(
#     min_rank("a"),
#     a = 5:1,
#     expected = 1L
#   )
#   check_not_hybrid_result(
#     percent_rank("a"),
#     a = 5:1,
#     expected = is.nan
#   )
#   check_not_hybrid_result(
#     cume_dist("a"),
#     a = 5:1,
#     expected = 1
#   )
#   check_not_hybrid_result(
#     dense_rank("a"),
#     a = 5:1,
#     expected = 1L
#   )
#   check_not_hybrid_result(
#     ntile("a", 2),
#     a = 5:1,
#     expected = 1L
#   )
#
#   expect_equal(
#     tibble(a = c(1, 1, 2), b = letters[1:3]) %>%
#       group_by(a) %>%
#       summarize(b = b[1], b = min_rank(desc(b))) %>%
#       ungroup(),
#     tibble(a = c(1, 2), b = c(1L, 1L))
#   )
# })
#
# test_that("hybrid handlers don't nest", {
#   check_not_hybrid_result(
#     mean(lag(a)),
#     a = 1:5,
#     expected = is.na
#   )
#   check_not_hybrid_result(
#     mean(row_number()),
#     a = 1:5,
#     expected = 3,
#     test_eval = FALSE
#   )
#   check_not_hybrid_result(
#     list(lag(cume_dist(a))),
#     a = 1:4,
#     expected = list(c(NA, 0.25, 0.5, 0.75))
#   )
# })
#
# test_that("row_number() is equivalent to dplyr::row_number() (#3309)", {
#   check_hybrid_result(
#     list(dplyr::row_number()),
#     a = 1:5,
#     expected = list(1:5),
#     test_eval = FALSE
#   )
#   expect_identical(
#     filter(mtcars, dplyr::row_number() == 6L),
#     filter(mtcars, row_number() == 6L)
#   )
# })
#
# test_that("constant folding and argument matching in hybrid evaluator (#2299)", {
#   skip("Currently failing")
#   skip("Currently failing (external var)")
#   c <- 1:3
#   check_not_hybrid_result(
#     n_distinct(c),
#     a = 1:5,
#     expected = 3L, test_eval = FALSE
#   )
#   check_not_hybrid_result(
#     n_distinct(a, c),
#     a = 1:3,
#     expected = 3L, test_eval = FALSE
#   )
#   check_not_hybrid_result(
#     n_distinct(a, b, na.rm = 1),
#     a = rep(1L, 3), b = c(1, 1, NA),
#     expected = 1L
#   )
#
#   skip("Currently failing (constfold)")
#   check_hybrid_result(
#     list(a %in% 1:3),
#     a = 2:4,
#     expected = list(c(TRUE, TRUE, FALSE))
#   )
#   check_hybrid_result(
#     list(a %in% as.numeric(1:3)),
#     a = as.numeric(2:4),
#     expected = list(c(TRUE, TRUE, FALSE))
#   )
#   check_hybrid_result(
#     list(a %in% letters[1:3]),
#     a = letters[2:4],
#     expected = list(c(TRUE, TRUE, FALSE))
#   )
#   check_hybrid_result(
#     list(a %in% c(TRUE, FALSE)),
#     a = c(TRUE, FALSE, NA),
#     expected = list(c(TRUE, TRUE, FALSE))
#   )
#
#   skip("Currently failing")
#   check_hybrid_result(
#     list(a %in% NA_integer_),
#     a = c(2:4, NA),
#     expected = list(c(FALSE, FALSE, FALSE, TRUE))
#   )
#   check_hybrid_result(
#     list(a %in% NA_real_),
#     a = as.numeric(c(2:4, NA)),
#     expected = list(c(FALSE, FALSE, FALSE, TRUE))
#   )
#   check_hybrid_result(
#     list(a %in% NA_character_),
#     a = c(letters[2:4], NA),
#     expected = list(c(FALSE, FALSE, FALSE, TRUE))
#   )
#   check_hybrid_result(
#     list(a %in% NA),
#     a = c(TRUE, FALSE, NA),
#     expected = list(c(FALSE, FALSE, TRUE))
#   )
#
#   skip("Currently failing (constfold)")
#   check_hybrid_result(
#     min(a, na.rm = (1 == 0)),
#     a = c(1:5, NA),
#     expected = NA_integer_
#   )
#   check_hybrid_result(
#     max(a, na.rm = (1 == 0)),
#     a = c(1:5, NA),
#     expected = NA_integer_
#   )
#   check_hybrid_result(
#     min(a, na.rm = (1 == 1)),
#     a = c(1:5, NA),
#     expected = 1L
#   )
#   check_hybrid_result(
#     max(a, na.rm = (1 == 1)),
#     a = c(1:5, NA),
#     expected = 5L
#   )
#
#   check_hybrid_result(
#     min(a, na.rm = pi != pi),
#     a = c(1:5, NA),
#     expected = NA_integer_
#   )
#   check_hybrid_result(
#     max(a, na.rm = pi != pi),
#     a = c(1:5, NA),
#     expected = NA_integer_
#   )
#   check_hybrid_result(
#     min(a, na.rm = pi == pi),
#     a = c(1:5, NA),
#     expected = 1L
#   )
#   check_hybrid_result(
#     max(a, na.rm = pi == pi),
#     a = c(1:5, NA),
#     expected = 5L
#   )
#
#   skip("Currently failing")
#   check_hybrid_result(
#     min(a, na.rm = F),
#     a = c(1:5, NA),
#     expected = NA_integer_
#   )
#   check_hybrid_result(
#     max(a, na.rm = F),
#     a = c(1:5, NA),
#     expected = NA_integer_
#   )
#   check_hybrid_result(
#     min(a, na.rm = T),
#     a = c(1:5, NA),
#     expected = 1L
#   )
#   check_hybrid_result(
#     max(a, na.rm = T),
#     a = c(1:5, NA),
#     expected = 5L
#   )
#
#   skip("Currently failing (constfold)")
#   check_hybrid_result(
#     nth(a, 1 + 2),
#     a = letters[1:5],
#     expected = "c"
#   )
#
#   check_hybrid_result(
#     nth(a, -4),
#     a = 1:5,
#     expected = 2L
#   )
#
#   skip("Currently failing (constfold)")
#   check_hybrid_result(
#     list(lead(a, 1L + 2L)),
#     a = 1:5,
#     expected = list(c(4:5, NA, NA, NA))
#   )
#   check_hybrid_result(
#     list(lag(a, 4L - 2L)),
#     a = as.numeric(1:5),
#     expected = list(c(NA, NA, as.numeric(1:3)))
#   )
#
#   check_not_hybrid_result(
#     list(lead(a, default = 2 + 4)),
#     a = 1:5,
#     expected = list(as.numeric(2:6))
#   )
#   check_not_hybrid_result(
#     list(lag(a, default = 3L - 3L)),
#     a = as.numeric(1:5),
#     expected = list(as.numeric(0:4))
#   )
#
#   check_hybrid_result(
#     list(lead(a, 1 + 2)),
#     a = 1:5,
#     expected = list(c(4:5, NA, NA, NA))
#   )
#   check_hybrid_result(
#     list(lag(a, 4 - 2)),
#     a = as.numeric(1:5),
#     expected = list(c(NA, NA, as.numeric(1:3)))
#   )
#
#   check_hybrid_result(
#     list(lead(a, default = 2L + 4L)),
#     a = 1:5,
#     expected = list(2:6)
#   )
#   check_hybrid_result(
#     list(lag(a, default = 3L - 3L)),
#     a = 1:5,
#     expected = list(0:4)
#   )
#
#   check_hybrid_result(
#     list(lead(a, def = 2L + 4L)),
#     a = 1:5,
#     expected = list(2:6)
#   )
#   check_hybrid_result(
#     list(lag(a, def = 3L - 3L)),
#     a = 1:5,
#     expected = list(0:4)
#   )
#
#   check_hybrid_result(
#     list(lead(a, 2, 2L + 4L)),
#     a = 1:5,
#     expected = list(c(3:6, 6L))
#   )
#   check_hybrid_result(
#     list(lag(a, 3, 3L - 3L)),
#     a = 1:5,
#     expected = list(c(0L, 0L, 0:2))
#   )
#
#   skip("Currently failing")
#   check_hybrid_result(
#     mean(a, na.rm = (1 == 0)),
#     a = c(1:5, NA),
#     expected = is.na
#   )
#   check_hybrid_result(
#     var(a, na.rm = (1 == 0)),
#     a = c(1:3, NA),
#     expected = is.na
#   )
#   check_hybrid_result(
#     sd(a, na.rm = (1 == 0)),
#     a = c(1:3, NA),
#     expected = is.na
#   )
#   check_hybrid_result(
#     sum(a, na.rm = (1 == 0)),
#     a = c(1:5, NA),
#     expected = NA_integer_
#   )
#   check_hybrid_result(
#     sum(a, na.rm = (1 == 0)),
#     a = c(as.numeric(1:5), NA),
#     expected = is.na
#   )
#
#   check_hybrid_result(
#     mean(a, na.rm = (1 == 1)),
#     a = c(1:5, NA),
#     expected = 3
#   )
#   check_hybrid_result(
#     var(a, na.rm = (1 == 1)),
#     a = c(1:3, NA),
#     expected = 1
#   )
#   check_hybrid_result(
#     sd(a, na.rm = (1 == 1)),
#     a = c(1:3, NA),
#     expected = 1
#   )
#   check_hybrid_result(
#     sum(a, na.rm = (1 == 1)),
#     a = c(1:5, NA),
#     expected = 15L
#   )
#   check_hybrid_result(
#     sum(a, na.rm = (1 == 1)),
#     a = c(as.numeric(1:5), NA),
#     expected = 15
#   )
#
#   check_hybrid_result(
#     mean(na.rm = (1 == 1), a),
#     a = c(1:5, NA),
#     expected = 3
#   )
#   check_hybrid_result(
#     var(na.rm = (1 == 1), a),
#     a = c(1:3, NA),
#     expected = 1
#   )
#   check_hybrid_result(
#     sd(na.rm = (1 == 1), a),
#     a = c(1:3, NA),
#     expected = 1
#   )
#   check_hybrid_result(
#     sum(na.rm = (1 == 1), a),
#     a = c(1:5, NA),
#     expected = 15L
#   )
#   check_hybrid_result(
#     sum(na.rm = (1 == 1), a),
#     a = c(as.numeric(1:5), NA),
#     expected = 15
#   )
#
#   skip("Currently failing (constfold)")
#   check_hybrid_result(
#     list(ntile(a, 1 + 2)),
#     a = c(1, 3, 2, 3, 1),
#     expected = list(c(1L, 2L, 2L, 3L, 1L))
#   )
#   check_hybrid_result(
#     list(ntile(a, 1L + 2L)),
#     a = c(1, 3, 2, 3, 1),
#     expected = list(c(1L, 2L, 2L, 3L, 1L))
#   )
#   check_hybrid_result(
#     list(ntile(n = 1 + 2, a)),
#     a = c(1, 3, 2, 3, 1),
#     expected = list(c(1L, 2L, 2L, 3L, 1L))
#   )
#
#   skip("Currently failing")
#   df <- data_frame(x = c(NA, 1L, 2L, NA, 3L, 4L, NA))
#   expected <- rep(4L, nrow(df))
#
#   expect_equal(mutate(df, y = last(na.omit(x)))$y,           expected)
#   expect_equal(mutate(df, y = last(x[!is.na(x)]))$y,         expected)
#   expect_equal(mutate(df, y = x %>% na.omit() %>% last())$y, expected)
#   expect_equal(mutate(df, y = x %>% na.omit() %>% last())$y, expected)
#
#   data_frame(x = c(1, 1)) %>%
#     mutate(y = 1) %>%
#     summarise(z = first(x, order_by = y))
# })
#
# test_that("simple handlers supports quosured symbols", {
#   expect_identical(
#     pull(summarise(mtcars, mean(!!quo(cyl)))),
#     base::mean(mtcars$cyl)
#   )
#   expect_identical(
#     pull(summarise(mtcars, sum(!!quo(cyl)))),
#     base::sum(mtcars$cyl)
#   )
#   expect_identical(
#     pull(summarise(mtcars, sd(!!quo(cyl)))),
#     stats::sd(mtcars$cyl)
#   )
#   expect_identical(
#     pull(summarise(mtcars, var(!!quo(cyl)))),
#     stats::var(mtcars$cyl)
#   )
# })
#
# test_that("%in% handler supports quosured symbols", {
#   expect_identical(
#     pull(mutate(mtcars, !!quo(cyl) %in% 4)),
#     base::`%in%`(mtcars$cyl, 4)
#   )
# })
#
# test_that("min() and max() handlers supports quosured symbols", {
#   expect_identical(
#     pull(summarise(mtcars, min(!!quo(cyl)))),
#     base::min(mtcars$cyl)
#   )
#   expect_identical(
#     pull(summarise(mtcars, max(!!quo(cyl)))),
#     base::max(mtcars$cyl)
#   )
# })
#
# test_that("lead/lag handlers support quosured symbols", {
#   expect_identical(
#     pull(mutate(mtcars, lead(!!quo(cyl)))),
#     dplyr::lead(mtcars$cyl)
#   )
#   expect_identical(
#     pull(mutate(mtcars, lag(!!quo(cyl)))),
#     dplyr::lag(mtcars$cyl)
#   )
# })
#
# test_that("window handlers supports quosured symbols", {
#   expect_identical(
#     pull(mutate(mtcars, ntile(!!quo(disp), 2))),
#     dplyr::ntile(mtcars$disp, 2)
#   )
#   expect_identical(
#     pull(mutate(mtcars, min_rank(!!quo(cyl)))),
#     dplyr::min_rank(mtcars$cyl)
#   )
#   expect_identical(
#     pull(mutate(mtcars, percent_rank(!!quo(cyl)))),
#     dplyr::percent_rank(mtcars$cyl)
#   )
#   expect_identical(
#     pull(mutate(mtcars, dense_rank(!!quo(cyl)))),
#     dplyr::dense_rank(mtcars$cyl)
#   )
#   expect_identical(
#     pull(mutate(mtcars, cume_dist(!!quo(cyl)))),
#     dplyr::cume_dist(mtcars$cyl)
#   )
# })
#
# test_that("n_distinct() handler supports quosured symbols", {
#   expect_identical(
#     pull(summarise(mtcars, n_distinct(!!quo(cyl)))),
#     dplyr::n_distinct(mtcars$cyl)
#   )
# })
#
# test_that("nth handlers support quosured symbols", {
#   expect_identical(
#     pull(summarise(mtcars, first(!!quo(cyl)))),
#     dplyr::first(mtcars$cyl)
#   )
#   expect_identical(
#     pull(summarise(mtcars, last(!!quo(cyl)))),
#     dplyr::last(mtcars$cyl)
#   )
#   expect_identical(
#     pull(summarise(mtcars, nth(!!quo(cyl), 2))),
#     dplyr::nth(mtcars$cyl, 2)
#   )
# })
#
# test_that("top_n() is hybridised (#2822)", {
#   expect_error(top_n(mtcars, 1, cyl), NA)
# })
#
# test_that("hybrid evaluation can be disabled locally (#3255)", {
#   tbl <- data.frame(x = 1:10)
#
#   first <- function(...) 42
#   expect_equal(summarise(tbl, y = first(x))$y, 42)
#   expect_equal(summarise(tbl, y = dplyr::first(x))$y, 1)
#
#   last <- function(...) 42
#   expect_equal(summarise(tbl, y = last(x))$y, 42)
#   expect_equal(summarise(tbl, y = dplyr::last(x))$y, 10)
#
#   nth <- function(...) 42
#   expect_equal(summarise(tbl, y = nth(x, 2L))$y, 42)
#   expect_equal(summarise(tbl, y = dplyr::nth(x, 2))$y, 2)
#
#   mean <- function(...) 42
#   tbl <- data.frame(x = 1:10)
#   expect_equal(summarise(tbl, y = mean(x))$y, 42)
#   expect_equal(summarise(tbl, y = base::mean(x))$y, 5.5)
#
#   var <- function(...) 42
#   expect_equal(summarise(tbl, y = var(x))$y, 42)
#   expect_equal(summarise(tbl, y = stats::var(x))$y, stats::var(tbl$x))
#
#   sd <- function(...) 42
#   expect_equal(summarise(tbl, y = sd(x))$y, 42)
#   expect_equal(summarise(tbl, y = stats::sd(x))$y, stats::sd(tbl$x))
#
#   row_number <- function() 42
#   expect_equal(mutate(tbl, y = row_number())$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::row_number())$y, 1:10)
#
#   ntile <- function(x, n) 42
#   expect_equal(mutate(tbl, y = ntile(x, 2))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::ntile(x, 2))$y, rep(1:2, each = 5))
#
#   min_rank <- function(x) 42
#   expect_equal(mutate(tbl, y = min_rank(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::min_rank(x))$y, 1:10)
#
#   percent_rank <- function(x) 42
#   expect_equal(mutate(tbl, y = percent_rank(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::percent_rank(x))$y, dplyr::percent_rank(1:10))
#
#   dense_rank <- function(x) 42
#   expect_equal(mutate(tbl, y = dense_rank(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::dense_rank(x))$y, dplyr::dense_rank(1:10))
#
#   cume_dist <- function(x) 42
#   expect_equal(mutate(tbl, y = cume_dist(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::cume_dist(x))$y, dplyr::cume_dist(1:10))
#
#   lead <- function(x) 42
#   expect_equal(mutate(tbl, y = lead(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::lead(x))$y, dplyr::lead(1:10))
#
#   lag <- function(x) 42
#   expect_equal(mutate(tbl, y = lag(x))$y, rep(42, 10))
#   expect_equal(mutate(tbl, y = dplyr::lag(x))$y, dplyr::lag(1:10))
#
#   `%in%` <- function(x, y) TRUE
#   expect_identical(filter(tbl, x %in% 3), tbl)
#
#   min <- function(x) 42
#   expect_equal(summarise(tbl, y = min(x))$y, 42)
#   expect_equal(summarise(tbl, y = base::min(x))$y, 1L)
#
#   max <- function(x) 42
#   expect_equal(summarise(tbl, y = max(x))$y, 42)
#   expect_equal(summarise(tbl, y = base::max(x))$y, 10L)
#
#   n <- function() 42
#   expect_equal(summarise(tbl, y = n())$y, 42)
#   expect_equal(summarise(tbl, y = dplyr::n())$y, 10L)
#
#   n_distinct <- function(x) 42
#   expect_equal(summarise(tbl, y = n_distinct(x))$y, 42)
#   expect_equal(summarise(tbl, y = dplyr::n_distinct(x))$y, 10L)
# })
#
# test_that("hybrid first and last fall back to R eval when no argument (#3589)", {
#   res <- mutate(tibble(v1 = 5:6), v2 = 1:4 %>% first(), v3 = 1:4 %>% last())
#   expect_equal(res$v2, rep(1L, 2))
#   expect_equal(res$v3, rep(4L, 2))
# })
