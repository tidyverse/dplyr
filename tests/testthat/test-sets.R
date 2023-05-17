test_that("x used as basis of output (#3839)", {
  df1 <- tibble(x = 1:4, y = 1)
  df2 <- tibble(y = 1, x = c(4, 2))

  expect_equal(intersect(df1, df2), tibble(x = c(2, 4), y = 1))
  expect_equal(union(df1, df2), tibble(x = 1:4, y = 1))
  expect_equal(union_all(df1, df2), tibble(x = c(1:4, 4, 2), y = 1))
  expect_equal(setdiff(df1, df2), tibble(x = c(1, 3), y = 1))
  expect_equal(symdiff(df1, df2), tibble(x = c(1, 3), y = 1))
})

test_that("set operations (apart from union_all) remove duplicates", {
  df1 <- tibble(x = c(1, 1, 2))
  df2 <- tibble(x = 2)

  expect_equal(intersect(df1, df2), tibble(x = 2))
  expect_equal(union(df1, df2), tibble(x = c(1, 2)))
  expect_equal(union_all(df1, df2), tibble(x = c(1, 1, 2, 2)))
  expect_equal(setdiff(df1, df2), tibble(x = 1))
  expect_equal(symdiff(df1, df2), tibble(x = 1))
})

test_that("standard coercion rules are used (#799)", {
  df1 <- tibble(x = 1:2, y = c(1, 1))
  df2 <- tibble(x = 1:2, y = 1:2)

  expect_equal(nrow(intersect(df1, df2)), 1)
  expect_equal(nrow(union(df1, df2)), 3)
  expect_equal(nrow(union_all(df1, df2)), 4)
  expect_equal(nrow(setdiff(df1, df2)), 1)
  expect_equal(nrow(symdiff(df1, df2)), 2)
})

test_that("grouping metadata is reconstructed (#3587)", {
  df1 <- tibble(x = 1:4, g = rep(1:2, each = 2)) %>% group_by(g)
  df2 <- tibble(x = 3:6, g = rep(2:3, each = 2))

  expect_equal(group_vars(intersect(df1, df2)), "g")
  expect_equal(group_vars(union(df1, df2)), "g")
  expect_equal(group_vars(union_all(df1, df2)), "g")
  expect_equal(group_vars(setdiff(df1, df2)), "g")
  expect_equal(group_vars(symdiff(df1, df2)), "g")
})

test_that("also work with vectors", {
  expect_equal(intersect(1:3, 3:4), 3)
  expect_equal(union(1:3, 3:4), 1:4)
  expect_equal(union_all(1:3, 3:4), c(1:3, 3:4))
  expect_equal(setdiff(1:3, 3:4), 1:2)
  expect_equal(symdiff(1:3, 3:4), c(1, 2, 4))
  # removes duplicates
  expect_equal(symdiff(c(1, 1, 2), c(2, 2, 3)), c(1, 3))
})

test_that("extra arguments in ... error (#5891)", {
  df1 <- tibble(var = 1:3)
  df2 <- tibble(var = 2:4)

  expect_snapshot(error = TRUE, {
    intersect(df1, df2, z = 3)
    union(df1, df2, z = 3)
    union_all(df1, df2, z = 3)
    setdiff(df1, df2, z = 3)
    symdiff(df1, df2, z = 3)
  })
})

test_that("incompatible data frames error (#903)", {
  df1 <- tibble(x = 1)
  df2 <- tibble(x = 1, y = 1)

  expect_snapshot(error = TRUE, {
    intersect(df1, df2)
    union(df1, df2)
    union_all(df1, df2)
    setdiff(df1, df2)
    symdiff(df1, df2)
  })
})

test_that("is_compatible generates useful messages for different cases", {
  expect_snapshot({
    cat(is_compatible(tibble(x = 1), 1))
    cat(is_compatible(tibble(x = 1), tibble(x = 1, y = 2)))
    cat(is_compatible(tibble(x = 1, y = 1), tibble(y = 1, x = 1), ignore_col_order = FALSE))
    cat(is_compatible(tibble(x = 1), tibble(y = 1)))
    cat(is_compatible(tibble(x = 1), tibble(x = 1L), convert = FALSE))
    cat(is_compatible(tibble(x = 1), tibble(x = "a")))
  })
})

# setequal ----------------------------------------------------------------

test_that("setequal ignores column and row order", {
  df1 <- tibble(x = 1:2, y = 3:4)
  df2 <- df1[2:1, 2:1]

  expect_true(setequal(df1, df2))
  expect_true(setequal(df1, df2))
})

test_that("setequal ignores duplicated rows (#6057)", {
  df1 <- tibble(x = 1)
  df2 <- df1[c(1, 1, 1), ]

  expect_true(setequal(df1, df2))
  expect_true(setequal(df2, df1))
})

test_that("setequal uses coercion rules (#6114)", {
  df1 <- tibble(x = 1)
  df2 <- tibble(x = 1L)

  expect_true(setequal(df1, df2))
  expect_true(setequal(df2, df1))
})

test_that("setequal tibbles must have same rows and columns", {
  # Different rows are the definition of not equal
  expect_false(setequal(tibble(x = 1:2), tibble(x = 2:3)))

  # Different or incompatible columns are an error, like the other set ops (#6786)
  expect_snapshot(error = TRUE, {
    setequal(tibble(x = 1:2), tibble(y = 1:2))
  })
  expect_snapshot(error = TRUE, {
    setequal(tibble(x = 1:2), tibble(x = c("a", "b")))
  })
})

test_that("setequal checks y is a data frame", {
  expect_snapshot(setequal(mtcars, 1), error = TRUE)
})

test_that("setequal checks for extra arguments", {
  expect_snapshot(setequal(mtcars, mtcars, z = 2), error = TRUE)
})

