context("test-split_by")

test_that("split_by works", {
  res <- split_by(iris, Species)
  gdata <- group_data(group_by(iris, Species))

  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(iris[gdata$.rows[[1]], ]), .groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(iris[gdata$.rows[[2]], ]), .groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(iris[gdata$.rows[[3]], ]), .groups = gdata[3, ]))

  res <- split_by_at(iris, vars("Species"))
  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(iris[gdata$.rows[[1]], ]), .groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(iris[gdata$.rows[[2]], ]), .groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(iris[gdata$.rows[[3]], ]), .groups = gdata[3, ]))

  res <- split_by_if(iris, is.factor)
  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(iris[gdata$.rows[[1]], ]), .groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(iris[gdata$.rows[[2]], ]), .groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(iris[gdata$.rows[[3]], ]), .groups = gdata[3, ]))
})

test_that("split_by respects empty groups", {
  viris <- filter(iris, Species %in% c("versicolor", "virginica"))

  res <- split_by(viris, Species)
  gdata <- group_data(group_by(viris, Species))

  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(viris[gdata$.rows[[1]], ]), .groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(viris[gdata$.rows[[2]], ]), .groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(viris[gdata$.rows[[3]], ]), .groups = gdata[3, ]))

  res <- split_by_at(viris, vars("Species"))
  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(viris[gdata$.rows[[1]], ]), .groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(viris[gdata$.rows[[2]], ]), .groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(viris[gdata$.rows[[3]], ]), .groups = gdata[3, ]))

  res <- split_by_if(viris, is.factor)
  expect_equal(length(res), 3L)
  expect_equal(res[[1]], structure(as_tibble(viris[gdata$.rows[[1]], ]), .groups = gdata[1, ]))
  expect_equal(res[[2]], structure(as_tibble(viris[gdata$.rows[[2]], ]), .groups = gdata[2, ]))
  expect_equal(res[[3]], structure(as_tibble(viris[gdata$.rows[[3]], ]), .groups = gdata[3, ]))
})
