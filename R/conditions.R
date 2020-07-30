arg_name <- function(quos, index) {
  name  <- names(quos)[index]
  if (name == "") {
    name <- glue("..{index}")
  }
  name
}

cnd_bullet_cur_group_label <- function() {
  label <- cur_group_label()
  if (label != "") {
    glue("The error occurred in {label}.")
  }
}

cnd_bullet_rowwise_unlist <- function() {
  data <- peek_mask()$full_data()
  if (inherits(data, "rowwise_df")) {
    glue("Did you mean: `{error_name} = list({error_expression})` ?", .envir = peek_call_step())
  }
}

or_1 <- function(x) {
  if(x == 1L) {
    "1"
  } else {
    glue("{x} or 1")
  }
}

# Common ------------------------------------------------------------------

local_call_step <- function(dots, .index, .fn, .dot_data = FALSE, frame = caller_env()) {
  error_expression  <- if (.dot_data) {
    deparse(quo_get_expr(dots[[.index]]))
  } else{
    as_label(quo_get_expr(dots[[.index]]))
  }
  context_local(
    "dplyr_call_step",
    env(
      error_name = arg_name(dots, .index),
      error_expression = error_expression,
      index = .index,
      dots = dots,
      fn = .fn,
      environment()
    ),
    frame = frame
  )
}
peek_call_step <- function() {
  context_peek("dplyr_call_step", "peek_call_step", "dplyr error handling")
}

cnd_bullet_header <- function() {
  glue("Problem with `{fn}()` input `{error_name}`.", .envir = peek_call_step())
}

cnd_bullet_input_info <- function(){
  glue("Input `{error_name}` is `{error_expression}`.", .envir = peek_call_step())
}

cnd_bullet_combine_details <- function(x, arg) {
  group <- as.integer(sub("^..", "", arg))
  keys <- group_keys(peek_mask()$full_data())[group, ]
  details <- group_labels_details(keys)
  glue("Result type for group {group} ({details}): <{vec_ptype_full(x)}>.")
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

stop_filter_incompatible_size <- function(index, dots, size, expected_size) {
  abort(glue("Input `..{index}` must be of size {or_1(expected_size)}, not size {size}."))
}

stop_filter_incompatible_type <- function(index, dots, index_column_name, result) {
  arg_name <- if (!is.null(index_column_name)) {
    glue("..{index}${index_column_name}")
  } else {
    glue("..{index}")
  }
  abort(glue("Input `{arg_name}` must be a logical vector, not a {vec_ptype_full(result)}."))
}

stop_filter_named <- function(index, expr, name) {
  abort(c(
    glue("Problem with `filter()` input `..{index}`."),
    x = glue("Input `..{index}` is named."),
    i = glue("This usually means that you've used `=` instead of `==`."),
    i = glue("Did you mean `{name} == {as_label(expr)}`?")
  ))
}

# summarise() -------------------------------------------------------------

stop_summarise_unsupported_type <- function(result) {
  abort(class = "dplyr:::summarise_unsupported_type", result = result)
}

# mutate() ----------------------------------------------------------------

stop_mutate_mixed_null <- function(index) {
  abort(class = "dplyr:::mutate_mixed_null")
}

stop_mutate_not_vector <- function(result, index, dots) {
  abort(class = "dplyr:::mutate_not_vector", result = result)
}

stop_mutate_recycle_incompatible_size <- function(x_size) {
  abort(class = "dplyr:::mutate_incompatible_size", x_size = x_size)
}

stop_summarise_incompatible_size <- function(group, index, expected_size, size) {
  # called from the C++ code
  abort(class = "dplyr:::summarise_incompatible_size", size = size, group = group, index = index, expected_size = expected_size)
}
