expect_propagate <- function(verb) {
  tryCatch(
    verb(data.frame(), rlang::abort("abcdef", class = "shiny.silent.error")),
    error = function(e) {
      expect_s3_class(e, "shiny.silent.error")
      expect_equal(conditionMessage(e), "abcdef")
    }
  )
}

test_that("verbs propagate shiny issues (#5552)", {
  expect_propagate(filter)
  expect_propagate(slice)
  expect_propagate(group_by)
  expect_propagate(mutate)
  expect_propagate(summarise)
  expect_propagate(arrange)
})
