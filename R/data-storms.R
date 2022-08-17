#' Storm tracks data
#'
#' This dataset is the NOAA Atlantic hurricane database best track data, <https://www.nhc.noaa.gov/data/#hurdat>.
#' The data includes the positions and attributes of storms from 1852-2021.
#' Storms from 1979 onward are measured every six hours during the lifetime of the storm. Storms in earlier years have lots of missing data, with very early storms often having only one data point.
#'
#' @seealso The script to create the storms data set: \url{https://github.com/tidyverse/dplyr/blob/main/data-raw/storms.R}
#'
#' @format A tibble with 22,184 observations and 13 variables:
#' \describe{
#' \item{name}{Storm Name}
#' \item{year,month,day}{Date of report}
#' \item{hour}{Hour of report (in UTC)}
#' \item{lat,long}{Location of storm center}
#' \item{status}{Storm classification (Tropical Depression, Tropical Storm,
#'   or Hurricane)}
#' \item{category}{Saffir-Simpson hurricane category calculated from wind speed.
#'   \itemize{
#'     \item category = NA : Not a hurricane
#'     \item category = 1 : 64+ knots
#'     \item category = 2 : 83+ knots
#'     \item category = 3 : 96+ knots
#'     \item category = 4 : 113+ knots
#'     \item category = 5 : 137+ knots
#'   }
#' }
#' \item{wind}{storm's maximum sustained wind speed (in knots)}
#' \item{pressure}{Air pressure at the storm's center (in millibars)}
#' \item{tropicalstorm_force_diameter}{Diameter (in nautical miles) of the area experiencing tropical storm strength winds (34 knots or above). Only available starting in 2004.}
#' \item{hurricane_force_diameter}{Diameter (in nautical miles) of the area experiencing hurricane strength winds (64 knots or above). Only available starting in 2004.}
#' }
#' @examples
#'
#' # Show a plot of the storm paths.
#' # Limit the storms to 1975 and later to ensure the figure fits.
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   storms %>%
#'     filter(year >= 1975) %>%
#'     ggplot() +
#'     aes(x=long, y=lat, color=paste(year, name)) +
#'     geom_path() +
#'     guides(color='none') +
#'     facet_wrap(~year)
#' }
#'
#' storms
"storms"
