#' Count unique combinations
#'
#' `n_distinct()` counts the number of unique/distinct combinations in a set
#' of one or more vectors. It's a faster and more concise equivalent to
#' `nrow(unique(data.frame(...)))`.
#'
#' @param ... Unnamed vectors. If multiple vectors are supplied, then they should
#'   have the same length. This function does technically respect the recycling
#'   rules, but since a constant vector won't affect the unique values,
#'   this is mostly of theoretical interest.
#' @param na.rm If `TRUE`, exclude missing values from the count.
#' @returns A single number.
#' @export
#' @examples
#' n_distinct()
#'
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
  check_dots_unnamed()
  data <- vctrs::data_frame(..., .name_repair = "minimal")

  if (isTRUE(na.rm)) {
    drop <- reduce(map(data, vec_equal_na), `|`)
    data <- vec_slice(data, !drop)
  }

  vec_unique_count(data)
}
