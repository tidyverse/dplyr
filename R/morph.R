#' @examples
#' mtcars %>% morph(vs = vs + am)
#'
#'
morph <- function(.data, ...) {
  cols <- mutate_cols(.data, ..., .track_usage = TRUE)

  used <- names(.data)[attr(cols, "usage")]
  cols[used] <- rep_along(used, list(NULL))

  dplyr_col_modify(.data, cols)
}
