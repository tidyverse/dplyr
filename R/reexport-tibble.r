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
#' See [format_glimpse()] for details on the formatting.
#'
#' @section S3 methods:
#' `glimpse` is an S3 generic with a customised method for `tbl`s and
#' `data.frames`, and a default method that calls [str()].
#'
#' @param x An object to glimpse at.
#' @param width Width of output: defaults to the setting of the
#'   `width` [option][pillar_options] (if finite)
#'   or the width of the console.
#' @param ... Unused, for extensibility.
#' @return x original x is (invisibly) returned, allowing `glimpse()` to be
#'   used within a data pipe line.
#'
#' @importFrom tibble glimpse
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

#' @importFrom tibble trunc_mat
#' @export
tibble::trunc_mat

#' @importFrom tibble tbl_sum
#' @export
tibble::tbl_sum
