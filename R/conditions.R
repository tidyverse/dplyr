glue_c <- function(..., .envir = caller_env()) {
  dots <- list2(...)
  dots <- dots[!map_lgl(dots, is.null)]
  dots <- map(dots, function(txt) set_names(as.character(glue(txt, .envir = .envir)), names(txt)))
  vec_c(!!!dots)
}

cur_group_label <- function(data) {
  UseMethod("cur_group_label")
}

cur_group_label.data.frame <- function(data) {
  NULL
}

cur_group_labels_details <- function(keys) {
  glue_collapse(map2_chr(keys, names(keys), function(x, name) {
    glue("{name} = {value}", value = format_v(x))
  }), ", ")
}

cur_group_label.grouped_df <- function(data) {
  group <- cur_group_id()
  details <- cur_group_labels_details(cur_group())
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

cnd_problem <- function(fn, what) {
  glue("`{fn}()` argument `{name}` {what}.", name = context_peek("error_name"))
}

cnd_bullet_current_expression <- function() {
  c(i = glue("`{error_name}` is `{error_expression}`.", .envir = context_env))
}

or_1 <- function(x) {
  should_be <- if(x == 1L) {
    "1"
  } else {
    glue("{x} or 1")
  }
}

set_error_context <- function(index, dots, .dot_data = FALSE) {
  expr  <- if (.dot_data) {
    deparse(quo_get_expr(dots[[index]]))
  } else{
    as_label(quo_get_expr(dots[[index]]))
  }
  name  <- arg_name(dots, index)
  context_poke("error_name", name)
  context_poke("error_expression", expr)
}

# Common ------------------------------------------------------------------

stop_dplyr <- function(.index, dots, fn, problem, ..., .dot_data = FALSE, .show_group_details = TRUE) {
  set_error_context(.index, dots, .dot_data = .dot_data)
  abort(glue_c(
    cnd_problem(fn, problem),
    cnd_bullet_current_expression(),
    if(.show_group_details) cnd_bullet_cur_group_label(),
    ...,

    .envir = caller_env()
  ))
}

stop_eval_tidy <- function(e, index, dots, fn) {
  stop_dplyr(index, dots, fn, "errored",
    x = conditionMessage(e)
  )
}

combine_details <- function(x, arg) {
  group <- as.integer(sub("^..", "", arg))
  keys <- group_keys(peek_mask()$full_data())[group, ]
  details <- cur_group_labels_details(keys)
  c(i = glue("Result type for group {group} ({details}) : <{vec_ptype_full(x)}>."))
}

stop_combine <- function(cnd, index, dots, fn = "summarise") {
  stop_dplyr(index, dots, fn, "must return compatible vectors across groups",
    combine_details(cnd$x, cnd$x_arg),
    combine_details(cnd$y, cnd$y_arg),
    .show_group_details = FALSE
  )
}

stop_error_data_pronoun_not_found <- function(msg, index, dots, fn = "summarise") {
  stop_dplyr(index, dots, fn, "errored", .dot_data = TRUE, x = msg)
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
    cnd_bullet_cur_group_label(),
    x = "It must be of size {expected_size} or 1, not size {size}."
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

  stop_dplyr(index, dots, "summarise", "must be a vector",
    x = "Result should be a vector, not {as_friendly_type(typeof(result))}."
  )
}

# mutate() ----------------------------------------------------------------

stop_mutate_mixed_NULL <- function(index, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_mutate_mixed_NULL")
  }

  stop_dplyr(index, dots, "mutate", "must return compatible vectors across groups",
    i = "Cannot combine NULL and non NULL results.",
    .show_group_details = FALSE
  )
}


stop_mutate_not_vector <- function(result, index, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_mutate_not_vector", result = result)
  }

  stop_dplyr(index, dots, "mutate", "must be a vector",
    x = "Result should be a vector, not {as_friendly_type(typeof(result))}."
  )
}

stop_mutate_recycle_incompatible_size <- function(cnd, index, dots) {
  stop_dplyr(index, dots, "mutate", "must be recyclable",
    x = conditionMessage(cnd)
  )
}

stop_summarise_incompatible_size <- function(size, group, index, expected_sizes, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_summarise_incompatible_size", size = size, group = group)
  }

  # so that cnd_bullet_cur_group_label() correctly reports the faulty group
  peek_mask()$set_current_group(group)

  stop_dplyr(index, dots, "summarise", "must be recyclable",
    x = "Result should be size {or_1(expected_sizes[group])}, not {size}.",
    i = "An earlier column had size {expected_sizes[group]}."
  )

}
