test_that("translation", {
  df <- data.frame(a = 1, b = 2)
  global <- 3

  expect_snapshot({
    rel_translate(quo(42))
    rel_translate(quo(42L))
    rel_translate(quo("fortytwo"))
    rel_translate(quo(TRUE))
    rel_translate(quo(a), df)
    rel_translate(quo(global), df)
    rel_translate(quo(a + 1), df)
    rel_translate(quo(a < b), df)
  })
})
