#' Efficiently count the number of unique values in a set of vectors
#'
#' This is a faster and more concise equivalent of `length(unique(x))`
#'
#' @param \dots vectors of values
#' @param na.rm if `TRUE` missing values don't count
#' @examples
#' x <- sample(1:10, 1e5, rep = TRUE)
#' length(unique(x))
#' n_distinct(x)
#' @export
n_distinct <- function(..., na.rm = FALSE) {
  data <- vctrs::data_frame(..., .name_repair = "minimal")

  if (isTRUE(na.rm)){
    data <- vec_slice(data, !reduce(map(data, vec_equal_na), `|`))
  }

  vec_unique_count(data)
}
