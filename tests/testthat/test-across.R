test_that("across() does not select grouping variables", {
  df <- data.frame(g = 1, x = 1)

  out <- df %>% group_by(g) %>% summarise(x = across(everything())) %>% pull()
  expect_equal(out, tibble(x = 1))
})

test_that("across() correctly names output columns automatically", {
  expect_named(
    tibble(x = 1, y = 2, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, mean)),
    c("x", "y")
  )
  expect_named(
    tibble(x = 1, y = 2, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, mean, names = "mean_{col}")),
    c("x", "mean_y")
  )
  expect_named(
    tibble(x = 1, y = 2, z = 3, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, list(mean = mean, sum = sum))),
    c("x", "y_mean", "y_sum", "z_mean", "z_sum")
  )
  expect_named(
    tibble(x = 1, y = 2, z = 3, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, list(mean = mean, sum))),
    c("x", "y_mean", "y_2", "z_mean", "z_2")
  )
  expect_named(
    tibble(x = 1, y = 2, z = 3, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, list(mean, sum = sum))),
    c("x", "y_1", "y_sum", "z_1", "z_sum")
  )
  expect_named(
    tibble(x = 1, y = 2, z = 3, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, list(mean, sum))),
    c("x", "y_1", "y_2", "z_1", "z_2")
  )
})

test_that("across() respect names= glue syntax", {
  expect_named(
    tibble(x = 1, y = 2, z = 3, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, mean, names = "mean_{col}")),
    c("x", "mean_y", "mean_z")
  )
  expect_named(
    tibble(x = 1, y = 2, z = 3, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, mean, names = "{fn}_{col}")),
    c("x", "1_y", "1_z")
  )
  expect_named(
    tibble(x = 1, y = 2, z = 3, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, list(mean = mean, sum = sum), names = "{fn}_{col}")),
    c("x", "mean_y", "sum_y", "mean_z", "sum_z")
  )
  expect_named(
    tibble(x = 1, y = 2, z = 3, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, list(mean = mean, sum), names = "{fn}_{col}")),
    c("x", "mean_y", "2_y", "mean_z", "2_z")
  )
  expect_named(
    tibble(x = 1, y = 2, z = 3, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, list(mean, sum = sum), names = "{fn}_{col}")),
    c("x", "1_y", "sum_y", "1_z", "sum_z")
  )
  expect_named(
    tibble(x = 1, y = 2, z = 3, g = "") %>%
      group_by(x) %>%
      summarise(across(is.numeric, list(mean, sum), names = "{fn}_{col}")),
    c("x", "1_y", "2_y", "1_z", "2_z")
  )
})

test_that("across() gives meaningful messages", {
  verify_output(test_path("test-across-errors.txt"), {
    tibble(x = 1) %>%
      summarise(res = across(is.numeric, 42))
  })
})
