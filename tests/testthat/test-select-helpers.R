context("select-helpers")

test_that("no set variables throws error", {
  expect_error(starts_with("z"), "Variable context not set")
})

test_that("failed match removes all columns", {
  old <- set_current_vars(c("x", "y"))
  on.exit(set_current_vars(old))

  expect_equal(starts_with("z"), integer(0))
  expect_equal(ends_with("z"), integer(0))
  expect_equal(contains("z"), integer(0))
  expect_equal(matches("z"), integer(0))
  expect_equal(num_range("z", 1:3), integer(0))
})


test_that("matches return integer positions", {
  old <- set_current_vars(c("abc", "acd", "bbc", "bbd", "eee"))
  on.exit(set_current_vars(old))

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

test_that("can use a variable", {
  vars <- "x"
  names(vars) <- vars

  expect_equal(select_vars(vars, starts_with(vars)), c(x = "x"))
  expect_equal(select_vars(vars, ends_with(vars)), c(x = "x"))
  expect_equal(select_vars(vars, contains(vars)), c(x = "x"))
  expect_equal(select_vars(vars, matches(vars)), c(x = "x"))
})

test_that("can use a variable even if it exists in the data (#2266)", {
  vars <- c("x", "y")
  names(vars) <- vars

  y <- "x"
  expected_result <- c(x = "x")

  expect_equal(select_vars(vars, starts_with(y)), expected_result)
  expect_equal(select_vars(vars, ends_with(y)), expected_result)
  expect_equal(select_vars(vars, contains(y)), expected_result)
  expect_equal(select_vars(vars, matches(y)), expected_result)
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
  expect_error(
    one_of(1L, vars = c("x", "y")),
    "Expected character arguments, got integer",
    fixed = TRUE
  )
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

test_that("one_of works with variables", {
  vars <- c("x", "y")
  expected_result <- c(x = "x")
  var <- "x"
  expect_equal(select_vars(vars, one_of(var)), expected_result)
  expect_error(select_vars(vars, one_of(`_x`)), "not found")
  expect_error(select_vars(vars, one_of(`_y`)), "not found")
})

test_that("one_of works when passed variable name matches the column name (#2266)", {
  vars <- c("x", "y")
  expected_result <- c(x = "x")
  x <- "x"
  y <- "x"
  expect_equal(select_vars(vars, one_of(!! x)), expected_result)
  expect_equal(select_vars(vars, one_of(!! y)), expected_result)
  expect_equal(select_vars(vars, one_of(y)), expected_result)
})

# first-selector ----------------------------------------------------------

test_that("initial (single) selector defaults correctly (issue #2275)", {
  cn <- setNames(nm = c("x", "y", "z"))

  ### Single Column Selected

  # single columns (present), explicit
  expect_equal(select_vars(cn, x), cn["x"])
  expect_equal(select_vars(cn, -x), cn[c("y", "z")])

  # single columns (present), matched
  expect_equal(select_vars(cn, contains("x")), cn["x"])
  expect_equal(select_vars(cn, -contains("x")), cn[c("y", "z")])

  # single columns (not present), explicit
  expect_error(select_vars(cn, foo), "object 'foo' not found")
  expect_error(select_vars(cn, -foo), "object 'foo' not found")

  # single columns (not present), matched
  expect_equal(select_vars(cn, contains("foo")), cn[integer()])
  expect_equal(select_vars(cn, -contains("foo")), cn)
})

test_that("initial (of multiple) selectors default correctly (issue #2275)", {
  cn <- setNames(nm = c("x", "y", "z"))

  ### Multiple Columns Selected

  # explicit(present) + matched(present)
  expect_equal(select_vars(cn, x, contains("y")), cn[c("x", "y")])
  expect_equal(select_vars(cn, x, -contains("y")), cn["x"])
  expect_equal(select_vars(cn, -x, contains("y")), cn[c("y", "z")])
  expect_equal(select_vars(cn, -x, -contains("y")), cn["z"])

  # explicit(present) + matched(not present)
  expect_equal(select_vars(cn, x, contains("foo")), cn["x"])
  expect_equal(select_vars(cn, x, -contains("foo")), cn["x"])
  expect_equal(select_vars(cn, -x, contains("foo")), cn[c("y", "z")])
  expect_equal(select_vars(cn, -x, -contains("foo")), cn[c("y", "z")])

  # matched(present) + explicit(present)
  expect_equal(select_vars(cn, contains("x"), y), cn[c("x", "y")])
  expect_equal(select_vars(cn, contains("x"), -y), cn["x"])
  expect_equal(select_vars(cn, -contains("x"), y), cn[c("y", "z")])
  expect_equal(select_vars(cn, -contains("x"), -y), cn["z"])

  # matched(not present) + explicit(not present)
  expect_error(select_vars(cn, contains("foo"), bar), "object 'bar' not found")
  expect_error(select_vars(cn, contains("foo"), -bar), "object 'bar' not found")
  expect_error(select_vars(cn, -contains("foo"), bar), "object 'bar' not found")
  expect_error(select_vars(cn, -contains("foo"), -bar), "object 'bar' not found")

  # matched(present) + matched(present)
  expect_equal(select_vars(cn, contains("x"), contains("y")), cn[c("x", "y")])
  expect_equal(select_vars(cn, contains("x"), -contains("y")), cn["x"])
  expect_equal(select_vars(cn, -contains("x"), contains("y")), cn[c("y", "z")])
  expect_equal(select_vars(cn, -contains("x"), -contains("y")), cn["z"])

  # matched(present) + matched(not present)
  expect_equal(select_vars(cn, contains("x"), contains("foo")), cn["x"])
  expect_equal(select_vars(cn, contains("x"), -contains("foo")), cn["x"])
  expect_equal(select_vars(cn, -contains("x"), contains("foo")), cn[c("y", "z")])
  expect_equal(select_vars(cn, -contains("x"), -contains("foo")), cn[c("y", "z")])

  # matched(not present) + matched(present)
  expect_equal(select_vars(cn, contains("foo"), contains("x")), cn["x"])
  expect_equal(select_vars(cn, contains("foo"), -contains("x")), cn[integer()])
  expect_equal(select_vars(cn, -contains("foo"), contains("x")), cn)
  expect_equal(select_vars(cn, -contains("foo"), -contains("x")), cn[c("y", "z")])

  # matched(not present) + matched(not present)
  expect_equal(select_vars(cn, contains("foo"), contains("bar")), cn[integer()])
  expect_equal(select_vars(cn, contains("foo"), -contains("bar")), cn[integer()])
  expect_equal(select_vars(cn, -contains("foo"), contains("bar")), cn)
  expect_equal(select_vars(cn, -contains("foo"), -contains("bar")), cn)
})

test_that("middle (no-match) selector should not clear previous selectors (issue #2275)", {
  cn <- setNames(nm = c("x", "y", "z"))

  expect_equal(
    select_vars(cn, contains("x"), contains("foo"), contains("z")),
    cn[c("x", "z")]
  )
  expect_equal(
    select_vars(cn, contains("x"), -contains("foo"), contains("z")),
    cn[c("x", "z")]
  )
})



# rename_vars -------------------------------------------------------------

test_that("when strict = FALSE, rename_vars always succeeds", {
  expect_error(
    rename_vars(c("a", "b"), d = e, strict = TRUE),
    "Argument `e`: unknown variables",
    fixed = TRUE
  )

  expect_equal(
    rename_vars(c("a", "b"), d = e, strict = FALSE),
    c("a" = "a", "b" = "b")
  )
})
