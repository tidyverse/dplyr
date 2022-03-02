test_that("can hide expression in error messages", {
  err <- catch_cnd(mutate(mtcars, invisible(999 + "")), "error")
  expect_false(grepl("999", cnd_header(err)))

  expect_snapshot(error = TRUE, {
    mutate(mtcars, invisible(999 + ""))
    summarise(mtcars, invisible(999 + ""))
    filter(mtcars, invisible(999 + ""))
    arrange(mtcars, invisible(999 + ""))
    select(mtcars, invisible(999 + ""))
    slice(mtcars, invisible(999 + ""))

    mutate(mtcars, var = invisible(999 + ""))
    summarise(mtcars, var = invisible(999 + ""))
    filter(mtcars, var = invisible(999 + ""))    # Named arg error
    arrange(mtcars, var = invisible(999 + ""))   # Suboptimal
    select(mtcars, var = invisible(999 + ""))
    slice(mtcars, var = invisible(999 + ""))
  })
})

test_that("can pass verb-level error call", {
  dplyr_local_error_call(call("foo"))
  expect_snapshot(error = TRUE, {
    mutate(mtcars, 1 + "")
    transmute(mtcars, 1 + "")
    summarise(mtcars, 1 + "")
    summarise(group_by(mtcars, cyl), 1 + "")
    filter(mtcars, 1 + "")
    arrange(mtcars, 1 + "")
    select(mtcars, 1 + "")
    slice(mtcars, 1 + "")
  })
})

test_that("can pass verb-level error call (example case)", {
  my_verb <- function(data, var1, var2) {
    dplyr_local_error_call()
    pull(transmute(data, .result = {{ var1 }} * {{ var2 }}))
  }
  expect_snapshot(error = TRUE, {
    my_verb(mtcars, 1 + "", am)
    my_verb(mtcars, cyl, c(am, vs))
  })
})

test_that("`err_locs()` works as expected", {
  expect_snapshot(error = TRUE, err_locs(1.5))
  expect_snapshot(error = TRUE, err_locs(integer()))

  expect_snapshot({
    err_locs(1L)
    err_locs(1:5)
    err_locs(1:6)
    err_locs(1:7)
  })
})
