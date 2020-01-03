stop_dplyr <- function(message = NULL, .subclass = NULL, ...) {
  abort(message, .subclass = c(.subclass, "dplyr_error"), ...)
}

#' @export
cnd_header.dplyr_error_filter_size <- function(cnd) {
  glue("`filter()` argument #{cnd$index_expression} is incorrect")
}

#' @export
cnd_body.dplyr_error_filter_size <- function(cnd) {
  bullets <- c(
    x = glue("It must be of size {cnd$expected_size} or 1, not size {cnd$size}")
  )
  if (is_grouped_df(cnd$data)) {
    bullets <- c(bullets,
      i = glue("The error occured in group {cnd$index_group}")
    )
  }
  format_error_bullets(bullets)
}

stop_filter_incompatible_size <- function(index_expression, index_group, size, expected_size, data) {
  stop_dplyr(.subclass = "dplyr_error_filter_size",
    index_expression = index_expression,
    index_group = index_group,
    size = size,
    expected_size = expected_size,
    data = data
  )
}


#' @export
cnd_header.dplyr_error_filter_type <- function(cnd) {
  if (cnd$index_column > 0) {
    glue("`filter()` argument #{cnd$index_expression} / column #{cnd$index_column} is incorrect")
  } else {
    glue("`filter()` argument #{cnd$index_expression} is incorrect")
  }

}

#' @export
cnd_body.dplyr_error_filter_type <- function(cnd) {
  bullets <- c(
    x = glue("It must be a logical vector, not a {vec_ptype_full(cnd$result)}")
  )
  if (is_grouped_df(cnd$data)) {
    bullets <- c(bullets,
      i = glue("The error occured in group {cnd$index_group}")
    )
  }
  format_error_bullets(bullets)
}

stop_filter_incompatible_type <- function(index_expression, index_column, index_group, result, data) {
  stop_dplyr(.subclass = "dplyr_error_filter_type",
    index_expression = index_expression,
    index_column = index_column,
    index_group = index_group,
    result = result,
    data = data
  )
}
