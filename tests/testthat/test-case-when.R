# ------------------------------------------------------------------------------
# case_when()

test_that("basic example is working", {
  x <- c(1, 2, 3)

  expect_identical(
    case_when(
      x <= 1, 1,
      x <= 2, 2,
      x <= 3, 3
    ),
    c(1, 2, 3)
  )
})

test_that("unhandled `NA` are propagated", {
  x <- c(NA, 0, 10)

  expect_identical(
    case_when(
      x > 2, 0,
      .default = 1
    ),
    c(NA, 1, 0)
  )
})

test_that("any `TRUE` overrides an `NA`", {
  x <- c(1, 2, NA, 3)
  expect <- c("one", "not_one", "missing", "not_one")

  # `TRUE` overriding before the `NA`
  expect_identical(
    case_when(
      is.na(x), "missing",
      x == 1, "one",
      .default = "not_one"
    ),
    expect
  )

  # `TRUE` overriding after the `NA`
  expect_identical(
    case_when(
      x == 1, "one",
      is.na(x), "missing",
      .default = "not_one"
    ),
    expect
  )
})

test_that("passes through `.default` correctly", {
  expect_identical(case_when(FALSE, 1, .default = 2), 2)
})

test_that("passes through `.ptype` correctly", {
  expect_identical(case_when(TRUE, 1, .ptype = integer()), 1L)
})

test_that("passes through `.size` correctly", {
  expect_snapshot(error = TRUE, {
    case_when(TRUE, 1, .size = 2)
  })
})

test_that("the new interface supports splicing in expressions that utilize the data mask", {
  fs <- exprs(
    cyl == 4, 1,
    am == 1, 2
  )

  out <- mutate(mtcars[1:4, ], out = case_when(!!!fs, .default = 0))
  out <- pull(out)

  expect_identical(out, c(2, 2, 1, 0))
})

test_that("doesn't support quosures in the new interface", {
  fs <- local({
    x <- 3:1
    quos(
      x < 2,
      1,
      x < 5,
      2
    )
  })

  # Tries to hand off to the old style interface because a quosure was detected
  expect_snapshot(error = TRUE, {
    case_when(!!!fs)
  })
})

test_that("invalid type errors are correct (#6261) (#6206)", {
  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 1, TRUE ~ "x")
  })
})

test_that("trying to mix the old interface with new arguments isn't allowed", {
  expect_snapshot(error = TRUE, case_when(1 ~ 2, .default = 3))
  expect_snapshot(error = TRUE, case_when(1 ~ 2, .ptype = integer()))
  expect_snapshot(error = TRUE, case_when(1 ~ 2, .size = 1))
})

# ------------------------------------------------------------------------------
# Pre-existing old interface tests

test_that("matches values in order", {
  x <- 1:3
  expect_equal(
    case_when(
      x <= 1 ~ 1,
      x <= 2 ~ 2,
      x <= 3 ~ 3
    ),
    c(1, 2, 3)
  )
})

test_that("unmatched gets missing value", {
  x <- 1:3
  expect_equal(
    case_when(
      x <= 1 ~ 1,
      x <= 2 ~ 2
    ),
    c(1, 2, NA)
  )
})

test_that("missing values can be replaced (#1999)", {
  x <- c(1:3, NA)
  expect_equal(
    case_when(
      x <= 1 ~ 1,
      x <= 2 ~ 2,
      is.na(x) ~ 0
    ),
    c(1, 2, NA, 0)
  )
})

test_that("NA conditions (#2927)", {
  expect_equal(
    case_when(
      c(TRUE, FALSE, NA) ~ 1:3,
      TRUE ~ 4L
    ),
    c(1L, 4L, 4L)
  )
})

test_that("atomic conditions (#2909)", {
  expect_equal(
    case_when(
      TRUE ~ 1:3,
      FALSE ~ 4:6
    ),
    1:3
  )
  expect_equal(
    case_when(
      NA ~ 1:3,
      TRUE ~ 4:6
    ),
    4:6
  )
})

test_that("zero-length conditions and values (#3041)", {
  expect_equal(
    case_when(
      TRUE ~ integer(),
      FALSE ~ integer()
    ),
    integer()
  )
  expect_equal(
    case_when(
      logical() ~ 1,
      logical() ~ 2
    ),
    numeric()
  )
})

test_that("case_when can be used in anonymous functions (#3422)", {
  res <- tibble(a = 1:3) %>%
    mutate(b = (function(x) case_when(x < 2 ~ TRUE, TRUE ~ FALSE))(a)) %>%
    pull()
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("case_when() can be used inside mutate()", {
  out <- mtcars[1:4, ] %>%
    mutate(out = case_when(
      cyl == 4            ~ 1,
      .data[["am"]] == 1  ~ 2,
      TRUE                ~ 0
    )) %>%
    pull()
  expect_identical(out, c(2, 2, 1, 0))
})

test_that("can pass quosures to case_when()", {
  fs <- local({
    x <- 3:1
    quos(
      x < 2 ~ TRUE,
      TRUE ~ FALSE
    )
  })
  expect_identical(case_when(!!!fs), c(FALSE, FALSE, TRUE))
})

test_that("can pass nested quosures to case_when()", {
  fs <- local({
    foo <- mtcars$cyl[1:4]
    quos(
      !!quo(foo) == 4 ~ 1,
      TRUE            ~ 0
    )
  })
  expect_identical(case_when(!!!fs), c(0, 0, 1, 0))
})

test_that("can pass unevaluated formulas to case_when()", {
  x <- 6:8
  fs <- exprs(
    x == 7L ~ TRUE,
    TRUE ~ FALSE
  )
  expect_identical(case_when(!!!fs), c(FALSE, TRUE, FALSE))

  out <- local({
    x <- 7:9
    case_when(!!!fs)
  })
  expect_identical(out, c(TRUE, FALSE, FALSE))
})

test_that("unevaluated formulas can refer to data mask", {
  fs <- exprs(
    cyl == 4 ~ 1,
    am == 1  ~ 2,
    TRUE     ~ 0
  )
  out <- mtcars[1:4, ] %>% mutate(out = case_when(!!!fs)) %>% pull()
  expect_identical(out, c(2, 2, 1, 0))
})

test_that("unevaluated formulas can contain quosures", {
  quo <- local({
    n <- 4
    quo(n)
  })
  fs <- exprs(
    cyl == !!quo ~ 1,
    am == 1      ~ 2,
    TRUE         ~ 0
  )
  out <- mtcars[1:4, ] %>% mutate(out = case_when(!!!fs)) %>% pull()
  expect_identical(out, c(2, 2, 1, 0))
})

test_that("NULL inputs are compacted", {
  x <- 1:3

  bool <- FALSE
  out <- case_when(
    x == 2           ~ TRUE,
    if (bool) x == 3 ~ NA,
    TRUE             ~ FALSE
  )
  expect_identical(out, c(FALSE, TRUE, FALSE))

  bool <- TRUE
  out <- case_when(
    x == 2           ~ TRUE,
    if (bool) x == 3 ~ NA,
    TRUE             ~ FALSE
  )
  expect_identical(out, c(FALSE, TRUE, NA))
})

test_that("case_when() give meaningful errors", {
  expect_snapshot({
    (expect_error(
      case_when(
        c(TRUE, FALSE) ~ 1:3,
        c(FALSE, TRUE) ~ 1:2
      )
    ))

    (expect_error(
      case_when(
        c(TRUE, FALSE) ~ 1,
        c(FALSE, TRUE, FALSE) ~ 2,
        c(FALSE, TRUE, FALSE, NA) ~ 3
      )
    ))

    (expect_error(
      case_when(50 ~ 1:3)
    ))
    (expect_error(
      case_when(paste(50))
    ))
    (expect_error(
      case_when()
    ))
    (expect_error(
      case_when(~1:2)
    ))
  })

})
