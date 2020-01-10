test_that("automatically finds common variables", {
  expect_message(vars <- join_vars2(c("x", "y"), c("x", "z"))$x$key)
  expect_named(vars, "x")
})

test_that("duplicate column names are given suffixes", {
  vars <- join_vars2(c("x", "y"), c("x", "y"), by = "x")
  expect_equal(vars$x$out, c("x" = 1, "y.x" = 2))
  expect_equal(vars$y$out, c("y.y" = 2))
})

test_that("by columns ommited from y" , {
  vars <- join_vars2(c("x", "y"), c("x", "y"), by = c("x" = "y"))
  expect_equal(vars$x$out, c("x" = 1, "y" = 2))
  expect_equal(vars$y$out, c("x.y" = 1))
})

test_that("emits useful messages", {
  verify_output(test_path("test-join-vars.txt"), {
    "# common by"
    xy <- c("x", "y")
    vars <- join_vars2(xy, xy)

    "# by errors"
    join_vars2(xy, c("a", "b"))

    join_vars2(xy, xy, by = FALSE)
    join_vars2(xy, xy, by = list(1, 2))
    join_vars2(xy, xy, by = c("x", "x"))
    join_vars2(xy, xy, by = c("x", NA))
    join_vars2(xy, xy, by = c("aaa", "bbb"))

    "# suffixes"
    join_vars2(xy, xy, by = "x", suffix = "x")
    join_vars2(xy, xy, by = "x", suffix = c("", NA))
    join_vars2(xy, xy, by = "x", suffix = c("", ""))
  })
})
