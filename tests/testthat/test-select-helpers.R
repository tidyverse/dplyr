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

test_that("initial (single) selector defaults correctly (issue #2275)", {
  cn <- setNames(nm = colnames(mtcars))

  ### Single Column Selected
  # single columns (present), explicit
  expect_equal(select_vars(cn, mpg), cn["mpg"])
  expect_equal(select_vars(cn, -mpg), cn[ cn != "mpg" ])
  # single columns (present), matched
  expect_equal(select_vars(cn, contains("mpg")), cn["mpg"])
  expect_equal(select_vars(cn, -contains("mpg")), cn[ cn != "mpg" ])
  # single columns (not present), explicit
  expect_error(select_vars(cn, foo), "object 'foo' not found")
  expect_error(select_vars(cn, -foo), "object 'foo' not found")
  # single columns (not present), matched
  res <- expect_named(select_vars(cn, contains("foo")))
  expect_length(res, 0)
  expect_equal(select_vars(cn, -contains("foo")), cn)
})

test_that("initial (of multiple) selectors default correctly (issue #2275)", {
  cn <- setNames(nm = colnames(mtcars))

  ### Multiple Columns Selected
  # explicit(present) + matched(present)
  expect_equal(select_vars(cn, mpg, contains("vs")), cn[c("mpg", "vs")])
  expect_equal(select_vars(cn, mpg, -contains("vs")), cn["mpg"])
  expect_equal(select_vars(cn, -mpg, contains("vs")), cn[ cn != "mpg" ])
  expect_equal(select_vars(cn, -mpg, -contains("vs")), cn[ ! cn %in% c("mpg", "vs") ])
  # explicit(present) + matched(not present)
  expect_equal(select_vars(cn, mpg, contains("foo")), cn["mpg"])
  expect_equal(select_vars(cn, mpg, -contains("foo")), cn["mpg"])
  expect_equal(select_vars(cn, -mpg, contains("foo")), cn[ cn != "mpg" ])
  expect_equal(select_vars(cn, -mpg, -contains("foo")), cn[ cn != "mpg" ])
  # matched(present) + explicit(present)
  expect_equal(select_vars(cn, contains("vs"), mpg), cn[c("vs", "mpg")])
  expect_equal(select_vars(cn, contains("vs"), -mpg), cn["vs"])
  expect_equal(select_vars(cn, -contains("vs"), mpg), cn[cn != "vs"])
  expect_equal(select_vars(cn, -contains("vs"), -mpg), cn[ ! cn %in% c("mpg", "vs") ])
  # matched(not present) + explicit(not present)
  expect_error(select_vars(cn, contains("foo"), bar), "object 'bar' not found")
  expect_error(select_vars(cn, contains("foo"), -bar), "object 'bar' not found")
  expect_error(select_vars(cn, -contains("foo"), bar), "object 'bar' not found")
  expect_error(select_vars(cn, -contains("foo"), -bar), "object 'bar' not found")
  # matched(present) + matched(present)
  expect_equal(select_vars(cn, contains("vs"), contains("mpg")), cn[c("vs", "mpg")])
  expect_equal(select_vars(cn, contains("vs"), -contains("mpg")), cn["vs"])
  expect_equal(select_vars(cn, -contains("vs"), contains("mpg")), cn[cn != "vs"])
  expect_equal(select_vars(cn, -contains("vs"), -contains("mpg")), cn[! cn %in% c("vs", "mpg")])
  # matched(present) + matched(not present)
  expect_equal(select_vars(cn, contains("vs"), contains("foo")), cn["vs"])
  expect_equal(select_vars(cn, contains("vs"), -contains("foo")), cn["vs"])
  expect_equal(select_vars(cn, -contains("vs"), contains("foo")), cn[cn != "vs"])
  expect_equal(select_vars(cn, -contains("vs"), -contains("foo")), cn[cn != "vs"])
  # matched(not present) + matched(present)
  expect_equal(select_vars(cn, contains("foo"), contains("mpg")), cn["mpg"])
  res <- expect_named(select_vars(cn, contains("foo"), -contains("mpg")))
  expect_length(res, 0)
  expect_equal(select_vars(cn, -contains("foo"), contains("mpg")), cn)
  expect_equal(select_vars(cn, -contains("foo"), -contains("mpg")), cn[cn != "mpg"])
  # matched(not present) + matched(not present)
  res <- expect_named(select_vars(cn, contains("foo"), contains("bar")))
  expect_length(res, 0)
  res <- expect_named(select_vars(cn, contains("foo"), -contains("bar")))
  expect_length(res, 0)
  expect_equal(select_vars(cn, -contains("foo"), contains("bar")), cn)
  expect_equal(select_vars(cn, -contains("foo"), -contains("bar")), cn)
})

test_that("middle (no-match) selector should not clear previous selectors (issue #2275)", {
  expect_equal(
    select_vars(colnames(mtcars), contains("am"), contains("foo"), contains("vs")),
    c(am = "am", vs = "vs")
  )
  expect_equal(
    select_vars(colnames(mtcars), contains("am"), -contains("foo"), contains("vs")),
    c(am = "am", vs = "vs")
  )
})
