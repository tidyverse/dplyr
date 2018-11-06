context("test-split_by")

test_that("split_by works", {
  res <- split_by(iris, Species)
  gdata <- group_data(group_by(iris, Species))

  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(iris[gdata$.rows[[1]], ]), groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(iris[gdata$.rows[[2]], ]), groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(iris[gdata$.rows[[3]], ]), groups = gdata[3, ]))

  res <- split_by_at(iris, vars("Species"))
  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(iris[gdata$.rows[[1]], ]), groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(iris[gdata$.rows[[2]], ]), groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(iris[gdata$.rows[[3]], ]), groups = gdata[3, ]))

  res <- split_by_if(iris, is.factor)
  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(iris[gdata$.rows[[1]], ]), groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(iris[gdata$.rows[[2]], ]), groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(iris[gdata$.rows[[3]], ]), groups = gdata[3, ]))
})

test_that("split_by respects empty groups", {
  viris <- filter(iris, Species %in% c("versicolor", "virginica"))

  res <- split_by(viris, Species)
  gdata <- group_data(group_by(viris, Species))

  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(viris[gdata$.rows[[1]], ]), groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(viris[gdata$.rows[[2]], ]), groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(viris[gdata$.rows[[3]], ]), groups = gdata[3, ]))

  res <- split_by_at(viris, vars("Species"))
  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(viris[gdata$.rows[[1]], ]), groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(viris[gdata$.rows[[2]], ]), groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(viris[gdata$.rows[[3]], ]), groups = gdata[3, ]))

  res <- split_by_if(viris, is.factor)
  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(viris[gdata$.rows[[1]], ]), groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(viris[gdata$.rows[[2]], ]), groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(viris[gdata$.rows[[3]], ]), groups = gdata[3, ]))
})

test_that("split.grouped_df() methods only works without arguments", {
  expect_error(split(group_by(mtcars, cyl), cyl), "split() on a grouped tibble is only supported without arguments, consider split_by()", fixed = TRUE)
  expect_error(split(rowwise(mtcars), cyl), "split() on a rowwise tibble is only supported without arguments, consider split_by()", fixed = TRUE)
})

test_that("split.grouped_df() works", {
  expect_equal(
    iris %>% group_by(Species) %>% split(),
    iris %>% split_by(Species)
  )
})

test_that("split.rowwise_df() works", {
  res <- iris %>% rowwise() %>% split()
  expect_equal(res, lapply(seq_len(nrow(iris)), function(.) as_tibble(iris[., ])))
})

test_that("split.tbl_df() aborts", {
  expect_error(split(tibble()), "split() not supported for tibbles, you probably need split_by()", fixed = TRUE)
})
