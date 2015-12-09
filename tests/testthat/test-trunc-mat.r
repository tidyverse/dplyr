context("Truncated matrix")

test_that("trunc_mat output matches known output", {
  expect_identical(
    capture.output(print(tbl_df(mtcars), n = 8L, width = 30L)),
    c("Source: local data frame [32 x 11]", "",
      "     mpg   cyl  disp    hp",
      "   (dbl) (dbl) (dbl) (dbl)",
      "1   21.0     6 160.0   110",
      "2   21.0     6 160.0   110",
      "3   22.8     4 108.0    93",
      "4   21.4     6 258.0   110",
      "5   18.7     8 360.0   175",
      "6   18.1     6 225.0   105",
      "7   14.3     8 360.0   245",
      "8   24.4     4 146.7    62",
      "..   ...   ...   ...   ...",
      "Variables not shown: drat",
      "  (dbl), wt (dbl), qsec",
      "  (dbl), vs (dbl), am (dbl),",
      "  gear (dbl), carb (dbl)."
    )
  )

  expect_identical(
    capture.output(print(tbl_df(iris), n = 5L, width = 30L)),
    c("Source: local data frame [150 x 5]", "",
      "   Sepal.Length Sepal.Width",
      "          (dbl)       (dbl)",
      "1           5.1         3.5",
      "2           4.9         3.0",
      "3           4.7         3.2",
      "4           4.6         3.1",
      "5           5.0         3.6",
      "..          ...         ...",
      "Variables not shown:",
      "  Petal.Length (dbl),",
      "  Petal.Width (dbl), Species",
      "  (fctr)."))

  expect_identical(
    capture.output(print(tbl_df(iris), n = 3L, width = 5L))[1:8],
    c("Source: local data frame [150 x 5]", "",
      "   Sepal.Length",
      "          (dbl)",
      "1           5.1",
      "2           4.9",
      "3           4.7",
      "..          ..."))

  expect_identical(
    capture.output(print(tbl_df(df_all), n = 5L, width = 30L)),
    c("Source: local data frame [2 x 7]", "",
      "      a     b     c     d",
      "  (dbl) (int) (lgl) (chr)",
      "1   1.0     1  TRUE     a",
      "2   2.5     2 FALSE     b",
      "Variables not shown: e",
      "  (fctr), f (date), g (time)."))
})
