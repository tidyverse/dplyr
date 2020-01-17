#' @examples
#' mtcars %>% as_tibble() %>% morph(vs2 = vs + am, mpgcyl = paste0(mpg, cyl))
#' mtcars %>% as_tibble() %>% morph(vs = vs + am)
morph <- function(.data, ...) {
  cols <- mutate_cols(.data, ..., .track_usage = TRUE)

  used <- setdiff(names(.data)[attr(cols, "usage")], names(cols))
  cols[used] <- rep_along(used, list(NULL))

  dplyr_col_modify(.data, cols)
}
