glue_c <- function(..., .envir = caller_env()) {
  map_chr(c(...), glue, .envir = .envir)
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

stop_eval_tidy <- function(e, index, quo, fn) {
  data  <- peek_mask()$full_data()
  group <- peek_mask()$get_current_group()

  abort(glue_c(
    "`{fn}()` argument `..{index}` errored",
    x = conditionMessage(e),
    i = "Expression being evaluated : {as_label(quo_get_expr(quo))}",
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

stop_summarise_unsupported_type <- function(result, index, quo) {
  # called from the C++ code
  if(missing(quo)) {
    abort(class = "dplyr_summarise_unsupported_type", result = result)
  }

  data  <- peek_mask()$full_data()
  group <- peek_mask()$get_current_group()

  # called again with context
  abort(glue_c(
    "`summarise()` argument `..{index}` incompatible",
    x = "Result should be a vector, not a `{typeof(result)}`",
    i = "..{index} is {as_label(quo_get_expr(quo))}",
    i = if(is_grouped_df(data)) "The error occured in group {group}"
  ))

}

stop_incompatible_size <- function(size, group, index, expected_sizes, quo) {
  # called from the C++ code
  if(missing(quo)) {
    abort(class = "dplyr_summarise_incompatible_size", size = size, group = group)
  }

  data  <- peek_mask()$full_data()

  # called again with context
  abort(glue_c(
    "`summarise()` argument `..{index}` incompatible",
    x = "Result should be size {expected_sizes[group]}, not {size}",
    i = "..{index} is {as_label(quo_get_expr(quo))}",
    i = if(is_grouped_df(data)) "The error occured in group {group}",
    i = paste0(
      "This happens when a previous expression gave a result of size {expected_sizes[group]}",
      if(is_grouped_df(data)) " for the group {group}" else ""
    )
  ))
}

stop_summarise_combine <- function(msg, index, quo) {
  abort(glue_c(
    "`summarise()` argument `..{index}` returns mixed types",
    x = "Error from vec_c() : {msg}",
    i = "..{index} is {as_label(quo_get_expr(quo))}"
  ))
}
