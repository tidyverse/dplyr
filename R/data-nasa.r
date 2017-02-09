#' NASA spatio-temporal data
#'
#' This data comes from the ASA 2007 data expo,
#' \url{http://stat-computing.org/dataexpo/2006/}. The data are geographic and
#' atmospheric measures on a very coarse 24 by 24 grid covering Central
#' America. The variables are: temperature (surface and air), ozone,
#' air pressure, and cloud cover (low, mid, and high). All variables are
#' monthly averages, with observations for Jan 1995 to Dec 2000. These data
#' were obtained from the NASA Langley Research Center Atmospheric Sciences
#' Data Center (with permission; see important copyright terms below).
#'
#' @section Dimensions:
#'
#' \itemize{
#'   \item `lat`, `long`: latitude and longitude
#'   \item `year`, `month`: month and year
#' }
#'
#' @section Measures:
#'
#' \itemize{
#'   \item `cloudlow`, `cloudmed`, `cloudhigh`: cloud cover
#'     at three heights
#'   \item `ozone`
#'   \item `surftemp` and `temperature`
#'   \item `pressure`
#' }
#' @docType data
#' @name nasa
#' @usage nasa
#' @format A [tbl_cube] with 41,472 observations.
#' @examples
#' nasa
NULL
