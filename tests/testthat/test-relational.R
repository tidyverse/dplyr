test_that("translation", {
  expect_snapshot({
    rel_translate(quo(42))
  })
})
