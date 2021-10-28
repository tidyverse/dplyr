#' Storm tracks data
#'
#' This data is a subset of the NOAA Atlantic hurricane database best track
#' data, \url{https://www.nhc.noaa.gov/data/#hurdat}. The data includes the
#' positions and attributes of storms from 1975-2020, measured every six hours
#' during the lifetime of a storm.
#'
#' @seealso The script to create the storms data set: \url{https://github.com/tidyverse/dplyr/blob/main/data-raw/storms.R}
#'
#' @format A tibble with 11,859 observations and 13 variables:
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
#' \item{tropicalstorm_force_diameter}{Diameter (in nautical miles) of the area experiencing tropical storm strength winds (34 knots or above)}
#' \item{hurricane_force_diameter}{Diameter (in nautical miles) of the area experiencing hurricane strength winds (64 knots or above)}
#' }
#' @examples
#'
#' # show a plot of the storm paths
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(storms) +
#'     aes(x=long, y=lat, color=paste(year, name)) +
#'     geom_path() +
#'     guides(color='none') +
#'     facet_wrap(~year)
#' }
#'
#' storms
"storms"
