#' Storm tracks data
#'
#' This data is a subset of the NOAA Atlantic hurricane database best track
#' data, \url{http://www.nhc.noaa.gov/data/#hurdat}. The data includes the
#' positions and attributes of 198 tropical storms, measured every six hours
#' during the lifetime of a storm.
#'
#' @format A tibble with 10,010 observations and 13 variables:
#' \describe{
#' \item{name}{Storm Name}
#' \item{year,month,day}{Date of report}
#' \item{hour}{Hour of report (in UTC)}
#' \item{lat,long}{Location of storm center}
#' \item{status}{Storm classification (Tropical Depression, Tropical Storm,
#'   or Hurricane)}
#' \item{category}{Saffir-Simpson storm category (estimated from wind speed.
#' -1 = Tropical Depression, 0 = Tropical Storm)}
#' \item{wind}{storm's maximum sustained wind speed (in knots)}
#' \item{pressure}{Air pressure at the storm's center (in millibars)}
#' \item{ts_diameter}{Diameter of the area experiencing tropical storm strength winds (34 knots or above)}
#' \item{hu_diameter}{Diameter of the area experiencing hurricane strength winds (64 knots or above)}
#' }
#' @examples
#' storms
"storms"
