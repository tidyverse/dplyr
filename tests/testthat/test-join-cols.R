test_that("automatically finds common variables", {
  expect_message(vars <- join_cols(c("x", "y"), c("x", "z")))
  expect_named(vars$x$key, "x")
  expect_named(vars$y$key, "x")
})

test_that("key vars are found", {
  vars <- join_cols(c("x", "y"), c("x", "z"), by = "x")
  expect_equal(vars$x$key, c(x = 1L))
  expect_equal(vars$y$key, c(x = 1L))

  vars <- join_cols(c("a", "x", "b"), c("x", "a"), by = "x")
  expect_equal(vars$x$key, c(x = 2L))
  expect_equal(vars$y$key, c(x = 1L))

  vars <- join_cols(c("x", "y"), c("a", "x", "z"), by = c("y" = "z"))
  expect_equal(vars$x$key, c(y = 2L))
  expect_equal(vars$y$key, c(y = 3L))
})

test_that("y key matches order and names of x key", {
  vars <- join_cols(c("x", "y", "z"), c("c", "b", "a"), by = c("x" = "a", "y" = "b"))
  expect_equal(vars$x$key, c(x = 1L, y = 2L))
  expect_equal(vars$y$key, c(x = 3L, y = 2L))
})

test_that("duplicate column names are given suffixes", {
  vars <- join_cols(c("x", "y"), c("x", "y"), by = "x")
  expect_equal(vars$x$out, c("x" = 1, "y.x" = 2))
  expect_equal(vars$y$out, c("y.y" = 2))

  # including join vars when keep = TRUE
  vars <- join_cols(c("x", "y"), c("x", "y"), by = "x", keep = TRUE)
  expect_equal(vars$x$out, c("x.x" = 1, "y.x" = 2))
  expect_equal(vars$y$out, c("x.y" = 1, "y.y" = 2))

  # suffixes don't create duplicates
  vars <- join_cols(c("x", "y", "y.x"), c("x", "y"), by = "x")
  expect_equal(vars$x$out, c("x" = 1, "y.x" = 2, "y.x.x" = 3))
  expect_equal(vars$y$out, c("y.y" = 2))

  # but not when they're the join vars
  vars <- join_cols(c("A", "A.x"), c("B", "A.x", "A"), by = "A.x")
  expect_named(vars$x$out, c("A.x.x", "A.x"))
  expect_named(vars$y$out, c("B", "A.y"))

  # or when no suffix is requested
  vars <- join_cols(c("x", "y"), c("x", "y"), by = "x", suffix = c("", ".y"))
  expect_equal(vars$x$out, c("x" = 1, "y" = 2))
  expect_equal(vars$y$out, c("y.y" = 2))
})

test_that("NA names are preserved", {
  vars <- join_cols(c("x", NA), c("x", "z"), by = "x")
  expect_named(vars$x$out, c("x", NA))

  vars <- join_cols(c("x", NA), c("x", NA), by = "x")
  expect_named(vars$x$out, c("x", "NA.x"))
  expect_named(vars$y$out, "NA.y")
})

test_that("by columns omited from y" , {
  vars <- join_cols(c("x", "y"), c("x", "y"), by = c("x" = "y"))
  expect_equal(vars$x$out, c("x" = 1, "y" = 2))
  expect_equal(vars$y$out, c("x.y" = 1))

  # unless specifically requested
  vars <- join_cols(c("x", "y"), c("x", "y"), by = c("x" = "y"), keep = TRUE)
  expect_equal(vars$x$out, c("x.x" = 1, "y.x" = 2))
  expect_equal(vars$y$out, c("x.y" = 1, "y.y" = 2))
})

test_that("emits useful messages", {
  verify_output(test_path("test-join-cols.txt"), {
    "# names"
    join_cols(c("x", "y"), c("y", "y"))
    join_cols(c("y", "y"), c("x", "y"))

    "# common by"
    xy <- c("x", "y")
    vars <- join_cols(xy, xy)

    "# by errors"
    join_cols(xy, c("a", "b"))

    join_cols(xy, xy, by = FALSE)
    join_cols(xy, xy, by = list(1, 2))
    join_cols(xy, xy, by = c("x", "x"))
    join_cols(xy, xy, by = c("x", NA))
    join_cols(xy, xy, by = c("aaa", "bbb"))

    "# suffixes"
    join_cols(xy, xy, by = "x", suffix = "x")
    join_cols(xy, xy, by = "x", suffix = c("", NA))
  })
})
