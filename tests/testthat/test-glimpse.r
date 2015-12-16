context("Glimpse")

test_that("glimpse output matches known output", {
  expect_identical(
    capture.output(glimpse(tbl_df(mtcars), width = 70L)),
    c("Observations: 32", "Variables: 11",
      "$ mpg  (dbl) 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8...",
      "$ cyl  (dbl) 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4...",
      "$ disp (dbl) 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146...",
      "$ hp   (dbl) 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, ...",
      "$ drat (dbl) 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92...",
      "$ wt   (dbl) 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.1...",
      "$ qsec (dbl) 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20....",
      "$ vs   (dbl) 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1...",
      "$ am   (dbl) 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1...",
      "$ gear (dbl) 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4...",
      "$ carb (dbl) 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1..."
    ))

  expect_identical(
    capture.output(glimpse(tbl_df(iris), width = 70L)),
    c("Observations: 150", "Variables: 5",
      "$ Sepal.Length (dbl) 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4,...",
      "$ Sepal.Width  (dbl) 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9,...",
      "$ Petal.Length (dbl) 1.4, 1.4, 1.3, 1.5, 1.4, 1.7, 1.4, 1.5, 1.4,...",
      "$ Petal.Width  (dbl) 0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.3, 0.2, 0.2,...",
      "$ Species      (fctr) setosa, setosa, setosa, setosa, setosa, set..."
    ))

  expect_identical(
    capture.output(glimpse(tbl_df(df_all), width = 70L)),
    c("Observations: 2", "Variables: 8",
      "$ a (dbl) 1.0, 2.5",
      "$ b (int) 1, 2",
      "$ c (lgl) TRUE, FALSE",
      "$ d (chr) \"a\", \"b\"",
      "$ e (fctr) a, b",
      "$ f (date) 2015-12-10, 2015-12-11",
      "$ g (time) 2015-12-09 10:51:35, 2015-12-09 10:51:36",
      "$ h (list) 1, 2"
    ))
})
