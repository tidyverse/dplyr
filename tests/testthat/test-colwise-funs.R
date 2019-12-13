test_that("as_fun_list() uses rlang auto-naming", {
  nms <- names(as_fun_list(list(min, max), env()))

  # Just check they are labellised as literals enclosed in brackets to
  # insulate from upstream changes
  expect_true(all(grepl("^<", nms)))
})
