#' Get a glimpse of your data
#'
#' @description
#' `glimpse()` is like a transposed version of `print()`:
#' columns run down the page, and data runs across.
#' This makes it possible to see every column in a data frame.
#' It's a little like [str()] applied to a data frame
#' but it tries to show you as much data as possible.
#' (And it always shows the underlying data, even when applied
#' to a remote data source.)
#'
#' `glimpse()` is provided by the pillar package, and re-exported
#' by dplyr. See [pillar::glimpse()] for more details.
#'
#' @return x original x is (invisibly) returned, allowing `glimpse()` to be
#'   used within a data pipeline.
#' @examples
#' glimpse(mtcars)
#'
#' # Note that original x is (invisibly) returned, allowing `glimpse()` to be
#' # used within a pipeline.
#' mtcars |>
#'   glimpse() |>
#'   select(1:3)
#'
#' glimpse(starwars)
#' @importFrom pillar glimpse
#' @export
#' @name glimpse
glimpse

#' @importFrom pillar type_sum
#' @export
pillar::type_sum
