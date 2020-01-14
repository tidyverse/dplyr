
#' EXperimental replacement for rowwise()
#'
#' @param .data A tibble
#' @param ... Expressions
#'
#' @examples
#' mtcars %>%
#' group_by(cyl) %>%
#'  summarise(fit = list(lm(gear ~ carb))) %>%
#'  linewise(broom::tidy(fit))
#' @export
linewise <- function(.data, ...) {
  tmp <- `class<-`(.data, c("rowwise_df", class(.data)))
  cols <- summarise_new_cols(tmp, ...)

  out <- .data[rep(seq_len(nrow(.data)), cols$size), ]
  out[names(cols$new)] <- cols$new
  out
}
