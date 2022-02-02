# dataframe ---------------------------------------------------------------

#' @importFrom tibble data_frame
#' @export
tibble::data_frame

#' @importFrom tibble data_frame_
#' @export
tibble::data_frame_

#' @importFrom tibble as_data_frame
#' @export
tibble::as_data_frame

#' @importFrom tibble lst
#' @export
tibble::lst

#' @importFrom tibble lst_
#' @export
tibble::lst_

#' @importFrom tibble add_row
#' @export
tibble::add_row

# type_sum ----------------------------------------------------------------

#' @importFrom tibble type_sum
#' @export
tibble::type_sum

# glimpse -----------------------------------------------------------------

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
#' mtcars %>%
#'   glimpse() %>%
#'   select(1:3)
#'
#' glimpse(starwars)
#' @importFrom pillar glimpse
#' @export
#' @name glimpse
glimpse

# frame-data --------------------------------------------------------------

#' @importFrom tibble frame_data
#' @export
tibble::frame_data

#' @importFrom tibble tribble
#' @export
tibble::tribble

#' @importFrom tibble tibble
#' @export
tibble::tibble

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' @importFrom tibble view
tibble::view

# utils -------------------------------------------------------------------

#' @importFrom tibble tbl_sum
#' @export
tibble::tbl_sum
