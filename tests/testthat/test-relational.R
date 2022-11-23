test_that("translation", {
  expect_snapshot({
    rel_translate(quo(42))
    rel_translate(quo(42L))
    rel_translate(quo("fortytwo"))
    rel_translate(quo(TRUE))
  })
})
