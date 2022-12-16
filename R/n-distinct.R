#' Count unique combinations
#'
#' `n_distinct()` counts the number of unique/distinct combinations in a set
#' of one or more vectors. It's a faster and more concise equivalent to
#' `nrow(unique(data.frame(...)))`.
#'
#' @param ... Unnamed vectors. If multiple vectors are supplied, then they should
#'   have the same length.
#' @param na.rm If `TRUE`, exclude missing observations from the count.
#'   If there are multiple vectors in `...`, an observation will
#'   be excluded if _any_ of the values are missing.
#' @returns A single number.
#' @export
#' @examples
#' x <- c(1, 1, 2, 2, 2)
#' n_distinct(x)
#'
#' y <- c(3, 3, NA, 3, 3)
#' n_distinct(y)
#' n_distinct(y, na.rm = TRUE)
#'
#' # Pairs (1, 3), (2, 3), and (2, NA) are distinct
#' n_distinct(x, y)
#'
#' # (2, NA) is dropped, leaving 2 distinct combinations
#' n_distinct(x, y, na.rm = TRUE)
#'
#' # Also works with data frames
#' n_distinct(data.frame(x, y))
n_distinct <- function(..., na.rm = FALSE) {
  if (missing(...)) {
    cli::cli_abort("{.arg ...} is absent, but must be supplied.")
  }
  check_dots_unnamed()

  data <- df_list(
    ...,
    .unpack = FALSE,
    .name_repair = "minimal",
    .error_call = current_env()
  )
  data <- new_data_frame(data)

  if (isTRUE(na.rm)) {
    # Drop observation if *any* missing
    complete <- vec_detect_complete(data)
    data <- vec_slice(data, complete)
  }

  vec_unique_count(data)
}
