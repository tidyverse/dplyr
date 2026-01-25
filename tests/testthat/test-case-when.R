# `case_when()` ----------------------------------------------------------------

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

test_that("any `TRUE` overrides an `NA`", {
  x <- c(1, 2, NA, 3)
  expect <- c("one", "not_one", "missing", "not_one")

  # `TRUE` overriding before the `NA`
  expect_identical(
    case_when(
      is.na(x) ~ "missing",
      x == 1 ~ "one",
      .default = "not_one"
    ),
    expect
  )

  # `TRUE` overriding after the `NA`
  expect_identical(
    case_when(
      x == 1 ~ "one",
      is.na(x) ~ "missing",
      .default = "not_one"
    ),
    expect
  )
})

test_that("case_when can be used in anonymous functions (#3422)", {
  res <- tibble(a = 1:3) |>
    mutate(b = (function(x) case_when(x < 2 ~ TRUE, .default = FALSE))(a)) |>
    pull()
  expect_equal(res, c(TRUE, FALSE, FALSE))
})

test_that("case_when() can be used inside mutate()", {
  out <- mtcars[1:4, ] |>
    mutate(
      out = case_when(
        cyl == 4 ~ 1,
        .data[["am"]] == 1 ~ 2,
        .default = 0
      )
    ) |>
    pull()
  expect_identical(out, c(2, 2, 1, 0))
})

test_that("case_when() conditions must be logical (and aren't cast to logical!)", {
  expect_snapshot(error = TRUE, {
    case_when(1 ~ 2)
  })
  # Make sure input numbering is right in the error message!
  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 2, 3.5 ~ 4)
  })
})

test_that("case_when() accepts logical condition vectors with attributes (#6678)", {
  x <- structure(c(FALSE, TRUE), label = "foo")
  expect_identical(case_when(x ~ 1, .default = 2), c(2, 1))
})

test_that("case_when() does not accept classed logical conditions", {
  # From a vctrs perspective, these aren't "logical condition indices"
  x <- structure(c(FALSE, TRUE), class = "my_logical")
  expect_snapshot(error = TRUE, {
    case_when(x ~ 1)
  })
})

test_that("case_when() logical conditions can't be arrays (#6862)", {
  x <- array(TRUE, dim = c(3, 3))
  y <- c("a", "b", "c")

  expect_snapshot(error = TRUE, {
    case_when(x ~ y)
  })

  # Not even 1D arrays
  x <- array(TRUE, dim = 3)

  expect_snapshot(error = TRUE, {
    case_when(x ~ y)
  })
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
      TRUE ~ 0
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
    am == 1 ~ 2,
    TRUE ~ 0
  )
  out <- mtcars[1:4, ] |> mutate(out = case_when(!!!fs)) |> pull()
  expect_identical(out, c(2, 2, 1, 0))
})

test_that("unevaluated formulas can contain quosures", {
  quo <- local({
    n <- 4
    quo(n)
  })
  fs <- exprs(
    cyl == !!quo ~ 1,
    am == 1 ~ 2,
    TRUE ~ 0
  )
  out <- mtcars[1:4, ] |> mutate(out = case_when(!!!fs)) |> pull()
  expect_identical(out, c(2, 2, 1, 0))
})

test_that("NULL inputs are compacted", {
  x <- 1:3

  bool <- FALSE
  out <- case_when(
    x == 2 ~ TRUE,
    if (bool) x == 3 ~ NA,
    .default = FALSE
  )
  expect_identical(out, c(FALSE, TRUE, FALSE))

  bool <- TRUE
  out <- case_when(
    x == 2 ~ TRUE,
    if (bool) x == 3 ~ NA,
    .default = FALSE
  )
  expect_identical(out, c(FALSE, TRUE, NA))
})

test_that("passes through `.default` correctly", {
  expect_identical(case_when(FALSE ~ 1, .default = 2), 2)
  expect_identical(
    case_when(c(TRUE, FALSE, TRUE, FALSE, TRUE) ~ 1:5, .default = 2),
    c(1, 2, 3, 2, 5)
  )
})

test_that("`.default` isn't part of recycling", {
  # Because eventually we want to only take the output size from the LHS conditions,
  # so having `.default` participate in the common size is a step in the wrong
  # direction
  expect_snapshot(error = TRUE, {
    case_when(FALSE ~ 1L, .default = 2:5)
  })
})

test_that("`.default` is part of common type computation", {
  expect_identical(case_when(TRUE ~ 1L, .default = 2), 1)

  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 1L, .default = "x")
  })
})

test_that("passes through `.ptype` correctly", {
  expect_identical(case_when(TRUE ~ 1, .ptype = integer()), 1L)

  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 1, FALSE ~ 1.5, .ptype = integer())
  })
  # Error index is right when `NULL` is involved
  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 1, NULL, FALSE ~ 1.5, .ptype = integer())
  })
})

test_that("passes through `.size` correctly", {
  expect_identical(case_when(TRUE ~ 1, .size = 2), c(1, 1))

  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 1:2, .size = 3)
  })
  # Error index is right when `NULL` is involved
  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 1:3, NULL, TRUE ~ 1:2, .size = 3)
  })
})

test_that("can't supply `.default` and `.unmatched`", {
  # Probably overkill to add `unmatched_arg` just to get `.unmatched` instead
  # of `unmatched`.
  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 1, .default = 1, .unmatched = "error")
  })
})

test_that("`.unmatched` is validated", {
  # Probably overkill to add `unmatched_arg` to `vec_case_when()` just to get
  # `.unmatched` instead of `unmatched`
  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 1, .unmatched = "foo")
  })
  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 1, .unmatched = 1)
  })
})

test_that("`.unmatched` treats `FALSE` like an unmatched location", {
  expect_snapshot(error = TRUE, {
    case_when(
      c(TRUE, FALSE, TRUE) ~ 1,
      .unmatched = "error"
    )
  })
})

test_that("`.unmatched` treats `NA` like an unmatched location", {
  expect_snapshot(error = TRUE, {
    case_when(
      c(TRUE, NA, TRUE) ~ 1,
      .unmatched = "error"
    )
  })
})

test_that("`.unmatched` errors pluralize well", {
  # One location
  x <- letters[1:5]
  expect_snapshot(error = TRUE, {
    case_when(
      x == "a" ~ 1,
      x == "b" ~ 2,
      x == "c" ~ 3,
      x == "e" ~ 4,
      .unmatched = "error"
    )
  })

  # Two locations
  x <- letters[1:5]
  expect_snapshot(error = TRUE, {
    case_when(
      x == "a" ~ 1,
      x == "c" ~ 2,
      x == "e" ~ 3,
      .unmatched = "error"
    )
  })

  # Many locations
  x <- 1:100
  expect_snapshot(error = TRUE, {
    case_when(x == 1 ~ "a", .unmatched = "error")
  })
})

# `case_when()` errors ---------------------------------------------------------

test_that("invalid type errors are correct (#6261) (#6206)", {
  expect_snapshot(error = TRUE, {
    case_when(TRUE ~ 1, TRUE ~ "x")
  })
})

test_that("`NULL` formula element throws meaningful error (#7794)", {
  # "Must be a vector" errors
  expect_snapshot(error = TRUE, {
    case_when(NULL ~ NULL)

    case_when(TRUE ~ NULL)
    case_when(NULL ~ TRUE)

    case_when(c(TRUE, TRUE) ~ NULL)
    case_when(NULL ~ c(TRUE, TRUE))

    case_when(TRUE ~ NULL, c(TRUE, TRUE) ~ NULL)
    case_when(NULL ~ TRUE, NULL ~ c(TRUE, TRUE))
  })

  # Recycling errors come first
  expect_snapshot(error = TRUE, {
    case_when(c(TRUE, TRUE) ~ NULL, c(TRUE, TRUE, TRUE) ~ NULL)
    case_when(NULL ~ c(TRUE, TRUE), NULL ~ c(TRUE, TRUE, TRUE))
  })
})

test_that("throws chained errors when formula evaluation fails", {
  expect_snapshot(error = TRUE, {
    case_when(1 ~ 2, 3 ~ stop("oh no!"))
  })
  expect_snapshot(error = TRUE, {
    case_when(1 ~ 2, stop("oh no!") ~ 4)
  })
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
      case_when(51:53 ~ 1:3)
    ))
    (expect_error(
      case_when(paste(50))
    ))
    (expect_error(
      case_when(y ~ x, paste(50))
    ))
    (expect_error(
      case_when()
    ))
    (expect_error(
      case_when(NULL)
    ))
    (expect_error(
      case_when(~ 1:2)
    ))
  })
})

# `case_when()` deprecated -----------------------------------------------------

test_that("Using scalar LHS with vector RHS is deprecated (#7082)", {
  # In many packages, people use `case_when()` when they should be using a
  # series of if statements. We try to warn when we detect this.
  expect_snapshot({
    # Columns
    x <- 1:5
    y <- 6:10

    # Scalars
    code <- 1L
    sex <- "M"

    # This is really a series of if statements.
    # This is highly inefficient because each scalar LHS is recycled to size 5.
    expect_identical(
      case_when(
        code == 1L && sex == "M" ~ x,
        code == 1L && sex == "F" ~ y,
        code == 1L && sex == "M" ~ x + 1L,
        .default = 0L
      ),
      x
    )
  })

  # Motivating example of a silent bug that results from allowing this kind of
  # common size determination (#7082). We ideally want this case to fail. LHS
  # common size is 1 and RHS inputs ideally should be forced to recycle to this
  # size. Since both the LHS and RHS inputs are consulted to compute a common
  # size of 0, this incorrectly returns `character()`, but we at least warn the
  # user that something is fishy here, and hopefully they take a closer look and
  # catch their error.
  expect_snapshot({
    x <- 1
    case_when(
      x == 1 ~ "a",
      x == 2 ~ character(),
      .default = "other"
    )
  })

  # Now confirm that the other 3 possible combinations don't warn!

  # size 1 LHS, size 1 RHS
  expect_identical(
    expect_no_warning(case_when(TRUE ~ "a", FALSE ~ "b")),
    "a"
  )
  # size >1 LHS, size 1 RHS
  expect_identical(
    expect_no_warning(case_when(c(TRUE, FALSE) ~ "a", c(FALSE, TRUE) ~ "b")),
    c("a", "b")
  )
  # size >1 LHS, size >1 RHS
  expect_identical(
    expect_no_warning(case_when(
      c(TRUE, FALSE) ~ c("a", "b"),
      c(FALSE, TRUE) ~ c("c", "d")
    )),
    c("a", "d")
  )
})

# `replace_when()` -------------------------------------------------------------

test_that("replace_when() recycles scalar RHS", {
  x <- c(1, 2, 3, 1, 2, 3)

  expect_identical(
    replace_when(x, x == 1 ~ 0, x == 3 ~ 4),
    c(0, 2, 4, 0, 2, 4)
  )
})

test_that("replace_when() allows vector RHS of the same size as `x`", {
  x <- c(1, 2, 3, 1, 2, 3)
  y <- seq_along(x)

  expect_identical(
    replace_when(x, x == 1 ~ 0, x == 3 ~ y),
    c(0, 2, 3, 0, 2, 6)
  )

  expect_snapshot(error = TRUE, {
    replace_when(x, x == 1 ~ 1:3)
  })
})

test_that("replace_when() does not recycle LHS values", {
  # Unlike `case_when()` we get to do this right!
  x <- c(1, 2, 3)

  expect_snapshot(error = TRUE, {
    replace_when(x, TRUE ~ 0)
  })

  # Error index is right when `NULL` is involved
  expect_snapshot(error = TRUE, {
    replace_when(x, c(TRUE, TRUE, TRUE) ~ 0, NULL, TRUE ~ 0)
  })
})

test_that("replace_when() retains the type of `x`", {
  x <- c(1L, 2L)

  # Not going towards common type of double
  expect_identical(
    replace_when(x, x == 1L ~ 0),
    c(0L, 2L)
  )

  x <- factor(c("a", "b", "c"))

  # Note common type would be character
  expect_identical(
    replace_when(x, x == "a" ~ "c"),
    factor(c("c", "b", "c"), levels = c("a", "b", "c"))
  )

  # Can't cast to unknown level
  expect_snapshot(error = TRUE, {
    replace_when(x, x == "a" ~ "d")
  })
  # Error index is right when `NULL` is involved
  expect_snapshot(error = TRUE, {
    replace_when(x, x == "a" ~ "b", NULL, x == "b" ~ "d")
  })
})

test_that("replace_when() retains names of `x`, consistent with `base::replace()`", {
  x <- c(a = 1, b = 2, c = 3)

  expect_identical(
    replace_when(
      x,
      x == 1 ~ 0,
      x == 3 ~ c(z = 4)
    ),
    c(a = 0, b = 2, c = 4)
  )

  # `x` does not become named if RHS inputs are named
  x <- c(1, 2, 3)

  expect_identical(
    replace_when(
      x,
      x == 1 ~ c(a = 0),
      x == 3 ~ c(b = 4)
    ),
    c(0, 2, 4)
  )
})

test_that("replace_when() does not allow named `...`", {
  # Purposefully stricter than `case_when()`
  expect_snapshot(error = TRUE, {
    replace_when(1, foo = TRUE ~ 2)
  })
})

test_that("replace_when() compacts `NULL` inputs", {
  expect_identical(
    replace_when(1, NULL, TRUE ~ 2, NULL),
    2
  )
})

test_that("replace_when() is a no-op with zero conditions", {
  # Unlike `case_when()`, where when zero conditions are supplied
  # we don't know what kind of vector to build (and we refuse to
  # build an `unspecified` vector, unlike `vec_case_when()`)
  expect_identical(replace_when(1), 1)
  expect_identical(replace_when(1, NULL), 1)
})

test_that("replace_when() works with data frames", {
  x <- tibble(a = c(1, 2, 3, 1), b = c(2, 3, 4, 2))

  expect_identical(
    replace_when(
      x,
      vec_equal(x, tibble(a = 1, b = 2)) ~ NA
    ),
    vec_assign(x, c(1, 4), NA)
  )

  expect_identical(
    replace_when(
      x,
      vec_equal(x, tibble(a = 1, b = 2)) ~ tibble(a = 0, b = -1)
    ),
    vec_assign(x, c(1, 4), tibble(a = 0, b = -1))
  )
})
