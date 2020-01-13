#' @export
linewise <- function(.data, ...) {
  tmp <- `class<-`(.data, c("rowwise_df", class(.data)))
  cols <- summarise_new_cols(tmp, ...)

  out <- .data[rep(seq_len(nrow(.data)), cols$size), ]
  out[names(cols$new)] <- cols$new
  out
}
