stop_dplyr <- function(message = NULL, .subclass = NULL, ...) {
  abort(message, .subclass = c(.subclass, "dplyr_error"), ...)
}

#' @export
cnd_header.dplyr_error_filter_size <- function(cnd) {
  glue("`filter()` argument `..{cnd$index_expression}` is incorrect")
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
  if (!is.null(cnd$index_column_name)) {
    glue("`filter()` argument `..{cnd$index_expression}${cnd$index_column_name}` is incorrect")
  } else {
    glue("`filter()` argument `..{cnd$index_expression}` is incorrect")
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

stop_filter_incompatible_type <- function(index_expression, index_column_name, index_group, result, data) {
  stop_dplyr(.subclass = "dplyr_error_filter_type",
    index_expression = index_expression,
    index_column_name = index_column_name,
    index_group = index_group,
    result = result,
    data = data
  )
}

#' @export
cnd_header.dplyr_error_filter_eval <- function(cnd) {
  glue("`filter()` argument `..{cnd$index_expression}` errored")
}

#' @export
cnd_body.dplyr_error_filter_eval <- function(cnd) {
  bullets <- c(x = cnd$message)
  if(is_grouped_df(cnd$data)){
    bullets <- c(bullets, c(i = glue("The error occured in group {cnd$group}")))
  }
  format_error_bullets(bullets)
}

stop_filter_eval_tidy <- function(e, index_expression) {
  stop_dplyr(
    conditionMessage(e),
    "dplyr_error_filter_eval",
    index_expression = index_expression,
    data = peek_mask()$full_data(),
    group = peek_mask()$get_current_group()
  )
}

#' @export
cnd_header.dplyr_error_filter_named <- function(cnd) {
  glue("`filter()` argument `..{cnd$index_expression}` is named")
}

#' @export
cnd_body.dplyr_error_filter_named <- function(cnd) {
  bullets <- c(
    i = "This usually means `=` is used insted of `==`",
    i = glue("Did you mean: `{names(cnd$quo)} == {as_label(cnd$quo[[1]])}`")
  )
  format_error_bullets(bullets)
}

stop_filter_named <- function(index_expression, quo) {
  stop_dplyr(
    .subclass = "dplyr_error_filter_named",
    index_expression = index_expression,
    quo = quo
  )
}
