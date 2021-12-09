test_that("can hide expression in error messages", {
  err <- catch_cnd(mutate(mtcars, invisible(999 + "")), "error")
  expect_false(grepl("999", conditionMessage(err)))

  expect_snapshot(error = TRUE, {
    mutate(mtcars, invisible(999 + ""))
    summarise(mtcars, invisible(999 + ""))
    filter(mtcars, invisible(999 + ""))
    arrange(mtcars, invisible(999 + ""))
    select(mtcars, invisible(999 + ""))

    mutate(mtcars, var = invisible(999 + ""))
    summarise(mtcars, var = invisible(999 + ""))
    filter(mtcars, var = invisible(999 + ""))    # Named arg error
    arrange(mtcars, var = invisible(999 + ""))   # Suboptimal
    select(mtcars, var = invisible(999 + ""))
  })
})

test_that("can hide variable name in error messages", {
  err <- catch_cnd(mutate(mtcars, .__foo__. = 999 + ""), "error")
  expect_false(grepl("foo", conditionMessage(err)))

  expect_snapshot(error = TRUE, {
    mutate(mtcars, .__var__. = 999 + "")
    summarise(mtcars, .__var__. = 999 + "")
    filter(mtcars, .__var__. = 999 + "")    # Named arg error
    arrange(mtcars, .__var__. = 999 + "")   # Suboptimal
    select(mtcars, .__var__. = 999 + "")
  })
})

test_that("can hide both variable name and expression in error messages", {
  expect_snapshot(error = TRUE, {
    mutate(mtcars, .__var__. = invisible(999 + ""))
    summarise(mtcars, .__var__. = invisible(999 + ""))
    filter(mtcars, .__var__. = invisible(999 + ""))    # Named arg error
    arrange(mtcars, .__var__. = invisible(999 + ""))   # Suboptimal
    select(mtcars, .__var__. = invisible(999 + ""))
  })
})
