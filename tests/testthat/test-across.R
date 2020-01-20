test_that("across() does not select grouping variables", {
  df <- data.frame(g = 1, x = 1)

  out <- df %>% group_by(g) %>% summarise(x = across(everything())) %>% pull()
  expect_equal(out, tibble(x = 1))
})

test_that("across() names the output after the selected columns by default in the single fn case", {
  expect_identical(
    iris %>%
      group_by(Species) %>%
      summarise(across(starts_with("Sepal"), mean)),

    iris %>%
      group_by(Species) %>%
      summarise(
        Sepal.Length = mean(Sepal.Length),
        Sepal.Width = mean(Sepal.Width)
      )
  )
})

test_that("across(names=) can control how to name the result columns in the single fn case", {
  expect_identical(
    iris %>%
      group_by(Species) %>%
      summarise(across(starts_with("Sepal"), mean, names = "mean_{col}")),

    iris %>%
      group_by(Species) %>%
      summarise(
        mean_Sepal.Length = mean(Sepal.Length),
        mean_Sepal.Width = mean(Sepal.Width)
      )
  )
})

test_that("across(names=) rejects use of {fn} in the single function case", {
  expect_error(
    iris %>%
      group_by(Species) %>%
      summarise(across(starts_with("Sepal"), mean, names = "{fn}_{col}"))
  )
})


test_that("across() automatically unpacks", {
  expect_identical(
    iris %>%
      group_by(Species) %>%
      summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd))),

    iris %>%
      group_by(Species) %>%
      summarise(
        Sepal.Length_mean = mean(Sepal.Length),
        Sepal.Length_sd = sd(Sepal.Length),
        Sepal.Width_mean = mean(Sepal.Width),
        Sepal.Width_sd = sd(Sepal.Width)
      )
  )
})

test_that("across() automatically names unnamed functions", {
  expect_identical(
    iris %>%
      group_by(Species) %>%
      summarise(across(starts_with("Sepal"), list(mean = mean, sd))),

    iris %>%
      group_by(Species) %>%
      summarise(
        Sepal.Length_mean = mean(Sepal.Length),
        Sepal.Length_2 = sd(Sepal.Length),
        Sepal.Width_mean = mean(Sepal.Width),
        Sepal.Width_2 = sd(Sepal.Width)
      )
  )

  expect_identical(
    iris %>%
      group_by(Species) %>%
      summarise(across(starts_with("Sepal"), list(mean, sd))),

    iris %>%
      group_by(Species) %>%
      summarise(
        Sepal.Length_1 = mean(Sepal.Length),
        Sepal.Length_2 = sd(Sepal.Length),
        Sepal.Width_1 = mean(Sepal.Width),
        Sepal.Width_2 = sd(Sepal.Width)
      )
  )
})

test_that("across(names=) can use {fn} and {col}", {
  expect_identical(
    iris %>%
      group_by(Species) %>%
      summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd), names = "{col}.{fn}")),

    iris %>%
      group_by(Species) %>%
      summarise(
        Sepal.Length.mean = mean(Sepal.Length),
        Sepal.Length.sd = sd(Sepal.Length),
        Sepal.Width.mean = mean(Sepal.Width),
        Sepal.Width.sd = sd(Sepal.Width)
      )
  )
})
