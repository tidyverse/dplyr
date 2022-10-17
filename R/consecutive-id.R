#' Generate a unique identifier for consecutive combinations
#'
#' `consecutive_id()` generates a unique identifier that increments every time
#' a variable (or combination of variables) changes. Inspired by
#' `data.table::rleid()`.
#'
#' @inheritParams n_distinct
#' @returns A numeric vector the same length as the longest
#'   element of `...`.
#' @export
#' @examples
#' consecutive_id(c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, NA, NA))
#' consecutive_id(c(1, 1, 1, 2, 1, 1, 2, 2))
#'
#' df <- data.frame(x = c(0, 0, 1, 0), y = c(2, 2, 2, 2))
#' df %>% group_by(x, y) %>% summarise(n = n())
#' df %>% group_by(id = consecutive_id(x, y), x, y) %>% summarise(n = n())
consecutive_id <- function(...) {
  check_dots_unnamed()

  data <- df_list(
    ...,
    .unpack = FALSE,
    .name_repair = "minimal",
    .error_call = current_env()
  )
  data <- new_data_frame(data)

  out <- vec_identify_runs(data)
  attr(out, "n") <- NULL

  out
}
