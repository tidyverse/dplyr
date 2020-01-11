glue_c <- function(..., .envir = caller_env()) {
  map_chr(c(...), glue, .envir = .envir)
}

err_vars <- function(x) {
  if (is.logical(x)) {
    x <- which(x)
  }
  if (is.character(x)) {
    x <- encodeString(x, quote = "`")
  }

  glue_collapse(x, sep = ", ", last = if (length(x) <= 2) " and " else ", and ")
}


stop_filter_incompatible_size <- function(index_expression, index_group, size, expected_size, data) {
  abort(glue_c(
    "`filter()` argument `..{index_expression}` is incorrect",
    x = "It must be of size {expected_size} or 1, not size {size}",
    i = if(is_grouped_df(data)) "The error occured in group {index_group}"
  ))
}

stop_filter_incompatible_type <- function(index_expression, index_column_name, index_group, result, data) {
  abort(glue_c(
    if (!is.null(index_column_name)) {
      "`filter()` argument `..{index_expression}${index_column_name}` is incorrect"
    } else {
      "`filter()` argument `..{index_expression}` is incorrect"
    },
    x = "It must be a logical vector, not a {vec_ptype_full(result)}",
    i = if(is_grouped_df(data)) "The error occured in group {index_group}"
  ))
}

stop_filter_eval_tidy <- function(e, index_expression) {
  data  <- peek_mask()$full_data()
  group <- peek_mask()$get_current_group()

  abort(glue_c(
    "`filter()` argument `..{index_expression}` errored",
    x = conditionMessage(e),
    i = if(is_grouped_df(data)) "The error occured in group {group}"
  ))
}

stop_filter_named <- function(index, expr, name) {
  abort(glue_c(
    "`filter()` argument `..{index}` is named",
    i = "This usually means that you've used `=` instead of `==`",
    i = "Did you mean `{name} == {as_label(expr)}` ?"
  ))
}
