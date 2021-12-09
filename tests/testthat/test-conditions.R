test_that("can hide expression in error messages", {
  err <- catch_cnd(mutate(mtcars, invisible(999 + "")), "error")
  expect_false(grepl("999", conditionMessage(err)))

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
  expect_snapshot(error = TRUE, {
    mutate(mtcars, 1 + "", .error_call = call("foo"))
    transmute(mtcars, 1 + "", .error_call = call("foo"))
    summarise(mtcars, 1 + "", .error_call = call("foo"))
    filter(mtcars, 1 + "", .error_call = call("foo"))
    arrange(mtcars, 1 + "", .error_call = call("foo"))
    select(mtcars, 1 + "", .error_call = call("foo"))
    slice(mtcars, 1 + "", .error_call = call("foo"))
  })
})

test_that("can pass verb-level error call (example case)", {
  my_verb <- function(data, var1, var2, error_call = current_env()) {
    pull(transmute(
      data,
      .result = {{ var1 }} * {{ var2 }},
      .error_call = error_call
    ))
  }
  expect_snapshot(error = TRUE, {
    my_verb(mtcars, 1 + "", am)
    my_verb(mtcars, cyl, c(am, vs))
  })
})
