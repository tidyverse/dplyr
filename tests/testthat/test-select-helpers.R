context("select-helpers")

test_that("failed match removes all columns", {
  set_current_vars(c("x", "y"))
  on.exit(reset_current_vars())

  expect_equal(starts_with("z"), -(1:2))
  expect_equal(ends_with("z"), -(1:2))
  expect_equal(contains("z"), -(1:2))
  expect_equal(matches("z"), -(1:2))
  expect_equal(num_range("z", 1:3), -(1:2))
})


test_that("matches return integer positions", {
  set_current_vars(c("abc", "acd", "bbc", "bbd", "eee"))
  on.exit(reset_current_vars())

  expect_equal(starts_with("a"), c(1L, 2L))
  expect_equal(ends_with("d"),   c(2L, 4L))
  expect_equal(contains("eee"),  5L)
  expect_equal(matches(".b."),   c(1L, 3L, 4L))
})

test_that("throws with empty pattern is provided", {
  expect_error(starts_with(""))
  expect_error(ends_with(""))
  expect_error(contains(""))
  expect_error(matches(""))
})

test_that("num_range selects numeric ranges", {
  vars <- c("x1", "x2", "x01", "x02", "x10", "x11")
  names(vars) <- vars

  expect_equal(select_vars(vars, num_range("x", 1:2)), vars[1:2])
  expect_equal(select_vars(vars, num_range("x", 1:2, width = 2)), vars[3:4])
  expect_equal(select_vars(vars, num_range("x", 10:11)), vars[5:6])
  expect_equal(select_vars(vars, num_range("x", 10:11, width = 2)), vars[5:6])
})


# one_of ------------------------------------------------------------------

test_that("one_of gives useful errors", {
  expect_error(one_of(1L, vars = c("x", "y")), "must be a character vector")
})

test_that("one_of tolerates but warns for unknown variables", {
  vars <- c("x", "y")

  expect_warning(res <- one_of("z", vars = vars), "Unknown variables: `z`")
  expect_equal(res, -(1:2))
  expect_warning(res <- one_of(c("x", "z"), vars = vars), "Unknown variables: `z`")
  expect_equal(res, 1L)

})

test_that("one_of converts names to positions", {
  expect_equal(one_of("a", "z", vars = letters), c(1L, 26L))
})
