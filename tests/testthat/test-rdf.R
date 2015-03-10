context("rdf")

test_that("'rdf' produces data.frames as expected", {

  df <- rdf(
    "a" | 1 | 3 | "b",
    "c" | 2 | 5 | "e"
  )

  expect_identical(df, data_frame(
    V1 = c("a", "c"),
    V2 = c(1, 2),
    V3 = c(3, 5),
    V4 = c("b", "e")
  ))

  df <- rdf(
    "A",
    "B",
    "C"
  )

  expect_identical(df, data_frame(
    V1 = c("A", "B", "C")
  ))

  expect_error(rdf(
    "a" | "b",
    "c" | "d" | "e"
  ))

  df <- rdf(
    a | b,
    c | d
  )

  expect_identical(df, data_frame(
    V1 = c("a", "c"),
    V2 = c("b", "d")
  ))

})
