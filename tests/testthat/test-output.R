context("output")

test_that("ungrouped output", {
  if (packageVersion("tibble") < "1.0-10")
    skip("need tibble 1.0-10 or later for this test")

  mtcars_mem <- src_memdb() %>% copy_to(mtcars, name = random_table_name())
  iris_mem <- src_memdb() %>% copy_to(iris, name = random_table_name())

  with_mock(
    `dplyr::sqlite_version` = function() "x.y.z",
    {
      expect_output_file_rel(
        print(mtcars_mem, n = 8L, width = 30L),
        "mtcars-8-30.txt")

      expect_output_file_rel(
        print(iris_mem, n = 5L, width = 30L),
        "iris-5-30.txt")

      expect_output_file_rel(
        print(iris_mem, n = 3L, width = 5L),
        "iris-3-5.txt")

      expect_output_file_rel(
        print(iris_mem, n = NULL, width = 70L),
        "iris--70.txt")

      expect_output_file_rel(
        print(iris_mem %>% head(), n = 30L, width = 80L),
        "iris-head-30-80.txt")
    }
  )
})
