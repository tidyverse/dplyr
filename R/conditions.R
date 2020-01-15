glue_c <- function(..., .envir = caller_env()) {
  map_chr(c(...), glue, .envir = .envir)
}

arg_name <- function(quos, index) {
  name  <- names(quos)[index]
  if (name == "") {
    name <- glue("..{index}")
  }
  name
}

# Common ------------------------------------------------------------------

stop_eval_tidy <- function(e, index, dots, fn) {
  data  <- peek_mask()$full_data()
  group <- peek_mask()$get_current_group()

  expr  <- as_label(quo_get_expr(dots[[index]]))
  name  <- arg_name(dots, index)

  abort(glue_c(
    "`{fn}()` argument `{name}` errored",
    x = conditionMessage(e),
    i = "`{name}` is {expr}",
    i = if(is_grouped_df(data)) "The error occured in group {group}"
  ))
}


stop_combine <- function(msg, index, dots, fn = "summarise") {
  name <- arg_name(dots, index)
  expr <- as_label(quo_get_expr(dots[[index]]))

  abort(glue_c(
    "`{fn}()` argument `{name}` must return compatible vectors across groups",
    x = "Error from vec_c() : {msg}",
    i = "`{name}` is {expr}"
  ))
}

stop_error_data_pronoun_not_found <- function(msg, index, dots, fn = "summarise") {
  name <- arg_name(dots, index)
  expr <- deparse(quo_get_expr(dots[[index]]))

  abort(glue_c(
    "`{fn}()` argument `{name}` must return compatible vectors across groups",
    x = msg,
    i = "`{name}` is {expr}"
  ))
}

# filter() ----------------------------------------------------------------

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

stop_filter_named <- function(index, expr, name) {
  abort(glue_c(
    "`filter()` argument `..{index}` is named",
    i = "This usually means that you've used `=` instead of `==`",
    i = "Did you mean `{name} == {as_label(expr)}` ?"
  ))
}

# summarise() -------------------------------------------------------------

stop_summarise_unsupported_type <- function(result, index, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_summarise_unsupported_type", result = result)
  }

  data  <- peek_mask()$full_data()
  group <- peek_mask()$get_current_group()
  expr  <- as_label(quo_get_expr(dots[[index]]))
  name  <- arg_name(dots, index)

  # called again with context
  abort(glue_c(
    "`summarise()` argument `{name}` must be a vector",
    x = "Result should be a vector, not {as_friendly_type(typeof(result))}",
    i = "`{name}` is {expr}",
    i = if(is_grouped_df(data)) "The error occured in group {group}"
  ))

}

# mutate() ----------------------------------------------------------------

stop_mutate_mixed_NULL <- function(index, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_mutate_mixed_NULL")
  }

  name <- arg_name(dots, index)
  expr <- as_label(quo_get_expr(dots[[index]]))

  abort(glue_c(
    "`mutate()` argument `{name}` must return compatible vectors across groups",
    i = "`{name}` is {expr}",
    i = "Cannot combine NULL and non NULL results"
  ))
}


stop_mutate_not_vector <- function(result, index, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_mutate_not_vector", result = result)
  }

  name <- arg_name(dots, index)
  expr <- as_label(quo_get_expr(dots[[index]]))
  data <- peek_mask()$full_data()
  group <- peek_mask()$get_current_group()

  abort(glue_c(
    "`mutate()` argument `{name}` must be a vector",
    i = "`{name}` is {expr}",
    x = "Result should be a vector, not {as_friendly_type(typeof(result))}",
    i = if(is_grouped_df(data)) "The error occured in group {group}"
  ))
}

stop_mutate_recycle_incompatible_size <- function(cnd, index, dots) {
  name <- arg_name(dots, index)
  expr <- as_label(quo_get_expr(dots[[index]]))
  data <- peek_mask()$full_data()
  group <- peek_mask()$get_current_group()

  abort(glue_c(
    "`mutate()` argument `{name}` must be recyclable",
    i = "`{name}` is {expr}",
    x = conditionMessage(cnd),
    i = if(is_grouped_df(data)) "The error occured in group {group}"
  ))
}

arg_name <- function(quos, index) {
  name  <- names(quos)[index]
  if (name == "") {
    name <- glue("..{index}")
  }
  name
}

stop_incompatible_size <- function(size, group, index, expected_sizes, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_summarise_incompatible_size", size = size, group = group)
  }

  data <- peek_mask()$full_data()
  name <- arg_name(dots, index)
  expr <- as_label(quo_get_expr(dots[[index]]))

  # called again with context

  should_be <- if(expected_sizes[group] == 1L) {
    "1"
  } else {
    glue("{expected_sizes[group]} or 1")
  }

  abort(glue_c(
    "`summarise()` argument `{name}` must be recyclable",
    x = "Result should be size {should_be}, not {size}",
    i = "`{name}` is {expr}",
    i = if(is_grouped_df(data)) "The error occured in group {group}",
    i = paste0(
      "An earlier column had size {expected_sizes[group]}",
      if(is_grouped_df(data)) " for the group {group}" else ""
    )
  ))
}
