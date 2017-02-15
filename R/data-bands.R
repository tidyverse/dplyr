#' Band membership
#'
#' These toy data sets describe band members of the Beatles and Rolling Stones.
#' They are designed to provide concise examples of dplyr joins that use data
#' that can be included in its entirety on a lecture slide.
#'
#' @format A tibble
#' @docType data
#' @name band
#' @usage band
#' @aliases band instrument instrument2
#' @examples
#' left_join(band, instrument, by = "name")
#' right_join(band, instrument, by = "name")
#' full_join(band, instrument, by = "name")
#' inner_join(band, instrument, by = "name")
#' semi_join(band, instrument, by = "name")
#' anti_join(band, instrument, by = "name")
#' left_join(band, instrument2, by = c("name" = "artist"))
NULL
