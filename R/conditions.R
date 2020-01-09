stop_dplyr <- function(.subclass = NULL, ...) {
  abort("", .subclass = c(.subclass, "dplyr_error"), ...)
}

#' @export
cnd_header.dplyr_error_filter_size <- function(cnd) {
  glue_data(cnd, "`filter()` argument `..{index_expression}` is incorrect")
}

#' @export
cnd_body.dplyr_error_filter_size <- function(cnd) {
  bullets <- c(
    x = glue_data(cnd, "It must be of size {expected_size} or 1, not size {size}")
  )
  if (is_grouped_df(cnd$data)) {
    bullets <- c(bullets,
      i = glue_data(cnd, "The error occured in group {index_group}")
    )
  }
  format_error_bullets(bullets)
}

stop_filter_incompatible_size <- function(index_expression, index_group, size, expected_size, data) {
  stop_dplyr("dplyr_error_filter_size",
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
    glue_data(cnd, "`filter()` argument `..{index_expression}${index_column_name}` is incorrect")
  } else {
    glue_data(cnd, "`filter()` argument `..{index_expression}` is incorrect")
  }

}

#' @export
cnd_body.dplyr_error_filter_type <- function(cnd) {
  bullets <- c(
    x = glue_data(cnd, "It must be a logical vector, not a {vec_ptype_full(result)}")
  )
  if (is_grouped_df(cnd$data)) {
    bullets <- c(bullets,
      i = glue_data(cnd, "The error occured in group {index_group}")
    )
  }
  format_error_bullets(bullets)
}

stop_filter_incompatible_type <- function(index_expression, index_column_name, index_group, result, data) {
  stop_dplyr("dplyr_error_filter_type",
    index_expression = index_expression,
    index_column_name = index_column_name,
    index_group = index_group,
    result = result,
    data = data
  )
}

#' @export
cnd_header.dplyr_error_filter_eval <- function(cnd) {
  glue_data(cnd, "`filter()` argument `..{index_expression}` errored")
}

#' @export
cnd_body.dplyr_error_filter_eval <- function(cnd) {
  bullets <- c(x = cnd$message)
  if(is_grouped_df(cnd$data)){
    bullets <- c(bullets, c(i = glue_data(cnd, "The error occured in group {group}")))
  }
  format_error_bullets(bullets)
}

stop_filter_eval_tidy <- function(e, index_expression) {
  stop_dplyr(
    "dplyr_error_filter_eval",
    message = conditionMessage(e),
    index_expression = index_expression,
    data = peek_mask()$full_data(),
    group = peek_mask()$get_current_group()
  )
}

#' @export
cnd_header.dplyr_error_filter_named <- function(cnd) {
  glue_data(cnd, "`filter()` argument `..{index}` is named")
}

#' @export
cnd_body.dplyr_error_filter_named <- function(cnd) {
  bullets <- c(
    i = "This usually means that you've used `=` instead of `==`",
    i = glue_data(cnd, "Did you mean `{name} == {as_label(expr)}` ?")
  )
  format_error_bullets(bullets)
}

stop_filter_named <- function(index, expr, name) {
  stop_dplyr(
    .subclass = "dplyr_error_filter_named",
    index = index,
    expr = expr,
    name = name
  )
}

#' @export
cnd_header.dplyr_error_select_corrupt <- function(cnd) {
  "`select()` received a corrupt `grouped_df` object"
}

#' @export
cnd_body.dplyr_error_select_corrupt <- function(cnd) {
  format_error_bullets(c(
    glue_data(cnd, "data appears to be grouped by `{col}`, but `{col}` is not a column")
  ))
}
