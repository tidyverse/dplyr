context("frame_data()")

test_that("frame_data() constructs 'data_frame' as expected", {

  result <- frame_data(
    ~colA, ~colB,
    "a", 1,
    "b", 2
  )

  compared <- data_frame(colA = c("a", "b"), colB = c(1, 2))
  expect_equal(result, compared)

})

test_that("frame_data() errs appropriately on bad calls", {

  # frame_data() must be passed colnames
  expect_error(frame_data(
    "a", "b",
    1, 2
  ))

  # frame_data() must produce rectangular structure (no filling)
  expect_error(frame_data(
    ~a, ~b, ~c,
    1, 2,
    3, 4, 5
  ))

})
