glue_c <- function(..., .envir = caller_env()) {
  map_chr(vec_c(...), glue, .envir = .envir)
}

cur_group_label <- function(data) {
  UseMethod("cur_group_label")
}

cur_group_label.data.frame <- function(data) {
  NULL
}

cur_group_label.grouped_df <- function(data) {
  group <- cur_group_id()
  keys <- cur_group()
  details <- glue_collapse(map2_chr(keys, names(keys), function(x, name) {
    glue("{name} = {value}", value = format_v(x))
  }), ", ")
  c(i = glue("The error occured in group {group}: {details}."))
}

cnd_bullet_cur_group_label <- function() {
  cur_group_label(peek_mask()$full_data())
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
  expr  <- as_label(quo_get_expr(dots[[index]]))
  name  <- arg_name(dots, index)

  abort(glue_c(
    "`{fn}()` argument `{name}` errored.",
    i = "`{name}` is {expr}",
    x = conditionMessage(e),
    cnd_bullet_cur_group_label()
  ))
}


stop_combine <- function(msg, index, dots, fn = "summarise") {
  name <- arg_name(dots, index)
  expr <- as_label(quo_get_expr(dots[[index]]))

  abort(glue_c(
    "`{fn}()` argument `{name}` must return compatible vectors across groups.",
    i = "`{name}` is {expr}",
    x = "Error from vec_c() : {msg}."
  ))
}

stop_error_data_pronoun_not_found <- function(msg, index, dots, fn = "summarise") {
  name <- arg_name(dots, index)
  # this instead of as_label() because it eats the ".data$"
  expr <- deparse(quo_get_expr(dots[[index]]))

  abort(glue_c(
    "`{fn}()` argument `{name}` errored.",
    i = "`{name}` is {expr}",
    cnd_bullet_cur_group_label(),
    x = msg
  ))
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

# filter() ----------------------------------------------------------------

stop_filter_incompatible_size <- function(index_expression, size, expected_size) {
  abort(glue_c(
    "`filter()` argument `..{index_expression}` is incorrect.",
    x = "It must be of size {expected_size} or 1, not size {size}.",
    cnd_bullet_cur_group_label()
  ))
}

stop_filter_incompatible_type <- function(index_expression, index_column_name, result) {
  abort(glue_c(
    if (!is.null(index_column_name)) {
      "`filter()` argument `..{index_expression}${index_column_name}` is incorrect."
    } else {
      "`filter()` argument `..{index_expression}` is incorrect."
    },
    x = "It must be a logical vector, not a {vec_ptype_full(result)}.",
    cnd_bullet_cur_group_label()
  ))
}

stop_filter_named <- function(index, expr, name) {
  abort(glue_c(
    "`filter()` argument `..{index}` is named.",
    i = "This usually means that you've used `=` instead of `==`.",
    i = "Did you mean `{name} == {as_label(expr)}` ?"
  ))
}

# summarise() -------------------------------------------------------------

stop_summarise_unsupported_type <- function(result, index, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_summarise_unsupported_type", result = result)
  }

  expr  <- as_label(quo_get_expr(dots[[index]]))
  name  <- arg_name(dots, index)

  # called again with context
  abort(glue_c(
    "`summarise()` argument `{name}` must be a vector.",
    i = "`{name}` is {expr}",
    cnd_bullet_cur_group_label(),
    x = "Result should be a vector, not {as_friendly_type(typeof(result))}."
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
    "`mutate()` argument `{name}` must return compatible vectors across groups.",
    i = "`{name}` is {expr}",
    i = "Cannot combine NULL and non NULL results."
  ))
}


stop_mutate_not_vector <- function(result, index, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_mutate_not_vector", result = result)
  }

  name <- arg_name(dots, index)
  expr <- as_label(quo_get_expr(dots[[index]]))

  abort(glue_c(
    "`mutate()` argument `{name}` must be a vector.",
    i = "`{name}` is {expr}.",
    x = "Result should be a vector, not {as_friendly_type(typeof(result))}.",
    cnd_bullet_cur_group_label()
  ))
}

stop_mutate_recycle_incompatible_size <- function(cnd, index, dots) {
  name <- arg_name(dots, index)
  expr <- as_label(quo_get_expr(dots[[index]]))

  abort(glue_c(
    "`mutate()` argument `{name}` must be recyclable.",
    i = "`{name}` is {expr}",
    x = conditionMessage(cnd),
    cnd_bullet_cur_group_label()
  ))
}

stop_summarise_incompatible_size <- function(size, group, index, expected_sizes, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_summarise_incompatible_size", size = size, group = group)
  }

  name <- arg_name(dots, index)
  expr <- as_label(quo_get_expr(dots[[index]]))

  # called again with context

  should_be <- if(expected_sizes[group] == 1L) {
    "1"
  } else {
    glue("{expected_sizes[group]} or 1")
  }

  # so that cnd_bullet_cur_group_label() correctly reports the faulty group
  peek_mask()$set_current_group(group)

  abort(glue_c(
    "`summarise()` argument `{name}` must be recyclable.",
    i = "`{name}` is {expr}",
    x = "Result should be size {should_be}, not {size}.",
    i = "An earlier column had size {expected_sizes[group]}.",
    cnd_bullet_cur_group_label()
  ))
}
