context("select-helpers")

test_that("failed match removes all columns", {
  set_current_vars(c("x", "y"))
  on.exit(reset_current_vars())

  expect_equal(starts_with("z"), integer(0))
  expect_equal(ends_with("z"), integer(0))
  expect_equal(contains("z"), integer(0))
  expect_equal(matches("z"), integer(0))
  expect_equal(num_range("z", 1:3), integer(0))
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
  expect_equal(res, integer(0))
  expect_warning(res <- one_of(c("x", "z"), vars = vars), "Unknown variables: `z`")
  expect_equal(res, 1L)

})

test_that("one_of converts names to positions", {
  expect_equal(one_of("a", "z", vars = letters), c(1L, 26L))
})

# first-selector ----------------------------------------------------------

test_that("mid-selector fails don't clear previous selectors (issue #2264)", {
  expect_equal(
    select_vars(colnames(mtcars), contains("am"), contains("FOO"), contains("vs")),
    c(am = "am", vs = "vs")
  )
})

test_that("initial selectors default correctly (issue #2264)", {
  # negative selector (exclusive)
  cn <- colnames(mtcars)
  expect_equal(
    select_vars(colnames(mtcars), -contains("FOO")),
    setNames(cn, nm = cn)
  )

  # positive selector (inclusive)
  expect_named(res <- select_vars(colnames(mtcars), contains("foo")))
  expect_length(res, 0)

  # ---------------------------------
  df <- data.frame(aa = 1, bb = 2, cc = 3)

  # ensure no errant reinclusion
  expect_equal(
    select(df, -contains("aa"), -contains("foo")),
    select(df, bb, cc)
  )

  # ensure no columns remain
  expect_is(res <- select(df, -(1:3), -contains("foo")), "data.frame")
  expect_equal(length(res), 0)
})
