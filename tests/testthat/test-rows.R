test_that("rows_insert()", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_insert(data, tibble(a = 4L, b = "z")),
    tibble(a = 1:4, b = c("a", "b", NA, "z"), c = c(0.5, 1.5, 2.5, NA))
  )

  expect_error(
    rows_insert(data, tibble(a = 3, b = "z")),
    class = "dplyr_rows_insert_duplicate"
  )

  expect_error(
    rows_insert(data, tibble(a = 4, b = c("y", "z"))),
    class = "dplyr_rows_duplicate"
  )
})

test_that("rows_update()", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_update(data, tibble(a = 2:3, b = "z")),
    tibble(a = 1:3, b = c("a", "z", "z"), c = data$c)
  )

  expect_error(
    rows_update(data, tibble(a = 2:3, b = "z"), by = c("a", "b")),
    class = "dplyr_rows_update_missing"
  )

  expect_silent(
    expect_identical(
      rows_update(data, tibble(b = "z", a = 2:3), by = "a"),
      tibble(a = 1:3, b = c("a", "z", "z"), c = data$c)
    )
  )
})

test_that("rows_patch()", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_patch(data, tibble(a = 2:3, b = "z")),
    tibble(a = 1:3, b = c("a", "b", "z"), c = data$c)
  )

  expect_error(
    rows_patch(data, tibble(a = 2:3, b = "z"), by = c("a", "b")),
    class = "dplyr_rows_patch_missing"
  )

  expect_silent(
    expect_identical(
      rows_patch(data, tibble(b = "z", a = 2:3), by = "a"),
      tibble(a = 1:3, b = c("a", "b", "z"), c = data$c)
    )
  )
})

test_that("rows_upsert()", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_upsert(data, tibble(a = 2:4, b = "z")),
    tibble(a = 1:4, b = c("a", "z", "z", "z"), c = c(data$c, NA))
  )
})

test_that("rows_delete()", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_delete(data, tibble(a = 2:3)),
    data[1, ]
  )

  expect_error(
    rows_delete(data, tibble(a = 2:4)),
    class = "dplyr_rows_delete_missing"
  )

  expect_identical(
    rows_delete(data, tibble(a = 2:3, b = "b")),
    data[1, ]
  )

  expect_error(
    rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b")),
    class = "dplyr_rows_delete_missing"
  )
})

verify_output("test-rows.txt", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)
  data

  "# Insert"
  rows_insert(data, tibble(a = 4, b = "z"))
  rows_insert(data, tibble(a = 3, b = "z"))

  "# Update"
  rows_update(data, tibble(a = 2:3, b = "z"))
  rows_update(data, tibble(a = 2:3, b = "z"), by = c("a", "b"))
  rows_update(data, tibble(b = "z", a = 2:3), by = "a")

  "# Variants: patch and upsert"
  rows_patch(data, tibble(a = 2:3, b = "z"))
  rows_patch(data, tibble(a = 2:3, b = "z"), by = c("a", "b"))
  rows_upsert(data, tibble(a = 2:4, b = "z"))

  "# Delete and truncate"
  rows_delete(data, tibble(a = 2:3))
  rows_delete(data, tibble(a = 2:4))
  rows_delete(data, tibble(a = 2:3, b = "b"))
  rows_delete(data, tibble(a = 2:3, b = "b"), by = c("a", "b"))

  "# Errors"
  rows_insert(data[c(1, 1), ], tibble(a = 3))
  rows_insert(data, tibble(a = c(4, 4)))

  rows_insert(data, tibble(d = 4))
  rows_insert(data, tibble(a = 4, b = "z"), by = "e")
})
