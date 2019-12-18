context("hybrid-traverse")

test_df <- tibble(
  id = c(1L, 2L, 2L),
  a = 1:3,
  b = as.numeric(1:3),
  c = letters[1:3],
  d = c(TRUE, FALSE, NA),
  e = list(list(a = 1, x = 2), list(a = 2, x = 3), list(a = 3, x = 4))
)

test_that("$ is parsed correctly (#1400)", {
  grouping <- rowwise

  expect_equal(
    test_df %>%
      grouping() %>%
      mutate(f = e$x) %>%
      select(-e),
    test_df %>%
      mutate(f = as.numeric(2:4)) %>%
      grouping() %>%
      select(-e)
  )
})

test_that("$ is parsed correctly if column by the same name exists (#1400)", {
  grouping <- rowwise

  expect_equal(
    test_df %>%
      grouping() %>%
      mutate(f = e$a) %>%
      select(-e),
    test_df %>%
      mutate(f = as.numeric(1:3)) %>%
      grouping() %>%
      select(-e)
  )
})

test_that("[[ works for ungrouped access (#912)", {
  grouping <- identity

  expect_equal(
    test_df %>%
      grouping() %>%
      mutate(f = mean(test_df[["a"]])) %>%
      select(-e),
    test_df %>%
      mutate(f = mean(a)) %>%
      grouping() %>%
      select(-e)
  )
})

test_that("[[ works for rowwise access of list columns (#912)", {
  grouping <- rowwise

  df <- tibble(
    x = c("a", "b"),
    y = list(list(a = 1, b = 2), list(a = 3, b = 4))
  )

  expect_equal(
    df %>% rowwise() %>% transmute(z = y[[x]]) %>% ungroup(),
    tibble(z = c(1, 4))
  )
})

test_that("$ works for rle result (#2125)", {
  grouping <- identity

  expect_equal(
    test_df %>%
      grouping() %>%
      mutate(f = rle(b)$lengths) %>%
      select(-e),
    test_df %>%
      mutate(f = rep(1L, 3L)) %>%
      grouping() %>%
      select(-e)
  )
})

test_hybrid <- function(grouping) {
  test_that("case_when() works for LHS (#1719, #2244)", {
    expect_equal(
      test_df %>%
        grouping() %>%
        mutate(f = case_when(a == 1 ~ 1, a == 2 ~ 2, TRUE ~ 3)) %>%
        select(-e),
      test_df %>%
        mutate(f = b) %>%
        grouping() %>%
        select(-e)
    )
  })

  test_that("case_when() works for RHS (#1719, #2244)", {
    expect_equal(
      test_df %>%
        grouping() %>%
        mutate(f = case_when(a == 1 ~ as.numeric(a), a == 2 ~ b, TRUE ~ 3)) %>%
        select(-e),
      test_df %>%
        mutate(f = b) %>%
        grouping() %>%
        select(-e)
    )
  })

  test_that("assignments work (#1452)", {
    expect_false(env_has(nms = "xx"))
    expect_equal(
      test_df %>%
        grouping() %>%
        mutate(f = {
          xx <- 5
          xx
        }) %>%
        select(-e),
      test_df %>%
        mutate(f = 5) %>%
        grouping() %>%
        select(-e)
    )
    expect_false(env_has(nms = "xx"))
  })

  test_that("assignments don't change variable (#315, #1452)", {
    expect_false(env_has(nms = "a"))
    expect_equal(
      test_df %>%
        grouping() %>%
        mutate(f = {
          a <- 5
          a
        }) %>%
        select(-e),
      test_df %>%
        mutate(f = 5) %>%
        grouping() %>%
        select(-e)
    )
    expect_false(env_has(nms = "a"))
  })

  test_that("assignments don't carry over (#1452)", {
    skip("being discussed in #3813")
    expect_error(
      test_df %>%
        grouping() %>%
        mutate(
          f = {
            xx <- 5
            xx
          },
          g = xx
        ),
      "xx"
    )
  })

  test_that("assignments don't leak (#1452)", {
    expect_false(env_has(nms = "a"))
    test <-
      test_df %>%
      grouping() %>%
      mutate(f = {
        xx <- 5
        xx
      })
    expect_false(env_has(nms = "a"))
  })

  test_that("[ works (#912)", {
    grouped_df <- test_df %>%
      grouping()

    expect_equal(
      grouped_df %>%
        mutate(f = mean(grouped_df["a"][[1]])) %>%
        select(-e),
      test_df %>%
        mutate(f = mean(a)) %>%
        grouping() %>%
        select(-e)
    )
  })

  test_that("interpolation works (#1012)", {
    var <- quo(b)

    expect_equal(
      test_df %>%
        grouping() %>%
        mutate(., f = mean(!!var)) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        mutate(f = mean(b)) %>%
        select(-e)
    )
  })

  test_that("can compute 1 - ecdf(y)(y) (#2018)", {
    surv <- function(x) 1 - ecdf(x)(x)

    expect_equal(
      test_df %>%
        grouping() %>%
        mutate(., f = 1 - ecdf(b)(b)) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        mutate(., f = surv(b)) %>%
        select(-e)
    )
  })

  test_that("filter understands .data (#1012)", {
    expect_equal(
      test_df %>%
        grouping() %>%
        filter({
          b <- 5
          .data$b < 2
        }) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        filter(b < 2) %>%
        select(-e)
    )
  })

  test_that("filter understands .data (#1012)", {
    expect_equal(
      test_df %>%
        grouping() %>%
        filter(.data[["b"]] < 2) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        filter(b < 2) %>%
        select(-e)
    )
  })

  test_that("filter understands .data (#1012)", {
    idx <- 2L

    expect_equal(
      test_df %>%
        grouping() %>%
        filter(.data[[letters[[idx]]]] < 2) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        filter(b < 2) %>%
        select(-e)
    )
  })

  test_that("filter understands .env (#1469)", {
    b <- 2L

    expect_equal(
      filter(
        test_df %>%
          grouping(),
        b < .env$b
      ) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        filter(b < 2) %>%
        select(-e)
    )
  })

  test_that("filter understands get(..., .env) in a pipe (#1469)", {
    b <- 2L

    expect_equal(
      test_df %>%
        grouping() %>%
        filter(b < get("b", envir = .env)) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        filter(b < 2) %>%
        select(-e)
    )
  })

  test_that("mutate understands .data (#1012)", {
    expect_equal(
      test_df %>%
        grouping() %>%
        mutate(f = {
          b <- 5
          .data$b
        }) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        mutate(f = b) %>%
        select(-e)
    )
  })

  test_that("mutate understands .data (#1012)", {
    expect_equal(
      test_df %>%
        grouping() %>%
        mutate(f = .data[["b"]]) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        mutate(f = b) %>%
        select(-e)
    )
  })

  test_that("mutate understands .data (#1012)", {
    idx <- 2L

    expect_equal(
      test_df %>%
        grouping() %>%
        mutate(f = .data[[letters[[idx]]]]) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        mutate(f = b) %>%
        select(-e)
    )
  })

  test_that("mutate understands .env (#1469)", {
    b <- 2L

    expect_equal(
      mutate(
        test_df %>%
          grouping(),
        f = .env$b
      ) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        mutate(f = 2L) %>%
        select(-e)
    )
  })

  test_that("mutate understands get(..., .env) in a pipe (#1469)", {
    b <- 2L

    expect_equal(
      test_df %>%
        grouping() %>%
        mutate(f = get("b", .env)) %>%
        select(-e),
      test_df %>%
        grouping() %>%
        mutate(f = 2L) %>%
        select(-e)
    )
  })

  test_that("summarise understands .data (#1012)", {
    expect_equal(
      test_df %>%
        grouping() %>%
        summarise(f = {
          b <- 5
          sum(.data$b)
        }),
      test_df %>%
        grouping() %>%
        summarise(f = sum(b))
    )
  })

  test_that("summarise understands .data (#1012)", {
    expect_equal(
      test_df %>%
        grouping() %>%
        summarise(f = sum(.data[["b"]])),
      test_df %>%
        grouping() %>%
        summarise(f = sum(b))
    )
  })

  test_that("summarise understands .data (#1012)", {
    idx <- 2L

    expect_equal(
      test_df %>%
        grouping() %>%
        summarise(f = sum(.data[[letters[[idx]]]])),
      test_df %>%
        grouping() %>%
        summarise(f = sum(b))
    )
  })

  test_that("summarise understands .env (#1469)", {
    b <- 2L

    expect_equal(
      summarise(
        test_df %>%
          grouping(),
        f = .env$b
      ),
      test_df %>%
        grouping() %>%
        summarise(f = 2L)
    )
  })

  test_that("summarise understands get(..., .env) in a pipe (#1469)", {
    b <- 2L

    expect_equal(
      test_df %>%
        grouping() %>%
        summarise(f = get("b", .env)),
      test_df %>%
        grouping() %>%
        summarise(f = 2L)
    )
  })

  test_that("columns named .data and .env are overridden", {
    conflict_data <- tibble(id = test_df$id, .data = 1:3, .env = 3:1)

    expect_equal(
      conflict_data %>%
        grouping() %>%
        summarise(env = list(.env), data = list(.data)) %>%
        ungroup() %>%
        summarise(
          is_env_env = all(vapply(env, is.environment, logical(1))),
          is_data_env = all(vapply(env, is.environment, logical(1)))
        ),
      tibble(is_env_env = TRUE, is_data_env = TRUE)
    )
  })

  test_that("contents of columns named .data and .env can be accessed", {
    conflict_data <- tibble(id = test_df$id, .data = 1:3, .env = 3:1)

    expect_equal(
      conflict_data %>%
        grouping() %>%
        summarise(
          env = mean(.data$.env),
          data = mean(.data$.data)
        ),
      conflict_data %>%
        set_names("id", "data", "env") %>%
        grouping() %>%
        summarise_at(vars(env, data), list(mean))
    )

    expect_equal(
      conflict_data %>%
        grouping() %>%
        summarise(
          env = mean(.data$.env),
          data = mean(.data$.data)
        ),
      conflict_data %>%
        set_names("id", "data", "env") %>%
        grouping() %>%
        summarise_at(vars(env, data), list(mean))
    )
  })
}

test_hybrid(identity)
test_hybrid(rowwise)
test_hybrid(. %>% group_by(!!quo(id)))
