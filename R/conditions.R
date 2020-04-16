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

cnd_problem <- function(fn, what) {
  glue("`{fn}()` argument `{name}` {what}.", name = context_peek("error_name"))
}

cnd_bullet_current_expression <- function() {
  c(i = glue("`{error_name}` is `{error_expression}`.", .envir = context_env))
}

cnd_bullet_cur_group_label <- function() {
  label <- cur_group_label()
  if (label != "") {
    c(i = glue("The error occured in {label}."))
  }
}

cnd_bullet_rowwise_unlist <- function() {
  data <- peek_mask()$full_data()
  if (inherits(data, "rowwise_df")) {
    c(i = "Did you mean: `{error_name} = list({error_expression})` ?")
  }
}

or_1 <- function(x) {
  if(x == 1L) {
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

stop_dplyr <- function(.index, dots, fn, problem, ..., .dot_data = FALSE, .show_group_details = TRUE, class = NULL) {
  set_error_context(.index, dots, .dot_data = .dot_data)
  envir <- env(
    error_name = context_peek("error_name"), error_expression = context_peek("error_expression"),
    caller_env()
  )

  abort(glue_c(
    cnd_problem(fn, problem),
    cnd_bullet_current_expression(),
    if(.show_group_details) cnd_bullet_cur_group_label(),
    ...,

    .envir = envir
    ),
    class = c(class, "dplyr_error"),
    error_name = context_peek("error_name"),
    error_expression = context_peek("error_expression")
  )
}

combine_details <- function(x, arg) {
  group <- as.integer(sub("^..", "", arg))
  keys <- group_keys(peek_mask()$full_data())[group, ]
  details <- group_labels_details(keys)
  c(i = glue("Result type for group {group} ({details}): <{vec_ptype_full(x)}>."))
}

stop_combine <- function(cnd, index, dots, fn = "summarise") {
  stop_dplyr(index, dots, fn, "must return compatible vectors across groups",
    combine_details(cnd$x, cnd$x_arg),
    combine_details(cnd$y, cnd$y_arg),
    .show_group_details = FALSE
  )
}

stop_error_data_pronoun_not_found <- function(msg, index, dots, fn = "summarise") {
  stop_dplyr(index, dots, fn, "errored", .dot_data = TRUE, x = msg,
    class = paste0("dplyr_error_", fn, "_data_pronoun_not_found")
  )
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
    x = "`..{index_expression}` must be of size {or_1(expected_size)}, not size {size}."
  ))
}

stop_filter_incompatible_type <- function(index_expression, index_column_name, result) {
  arg_name <- if (!is.null(index_column_name)) {
    glue("..{index_expression}${index_column_name}")
  } else {
    glue("..{index_expression}")
  }
  abort(glue_c(
    "`filter()` argument `{arg_name}` is incorrect.",
    cnd_bullet_cur_group_label(),
    x = "`{arg_name}` must be a logical vector, not a {vec_ptype_full(result)}."
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
    x = "`{error_name}` must be a vector, not {friendly_type_of(result)}.",
    cnd_bullet_rowwise_unlist()
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
    .show_group_details = FALSE,
    cnd_bullet_rowwise_unlist()
  )
}


stop_mutate_not_vector <- function(result, index, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_mutate_not_vector", result = result)
  }

  stop_dplyr(index, dots, "mutate", "must be a vector",
    x = "`{error_name}` must be a vector, not {friendly_type_of(result)}.",
    cnd_bullet_rowwise_unlist()
  )
}

stop_mutate_recycle_incompatible_size <- function(cnd, index, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_mutate_incompatible_size", x_size = cnd)
  }

  stop_dplyr(index, dots, "mutate", "must be recyclable",
    x = "`{error_name}` can't be recycled to size {cnd$size}.",
    i = "`{error_name}` must be size {or_1(cnd$size)}, not {cnd$x_size}.",
    cnd_bullet_rowwise_unlist()
  )
}

stop_summarise_incompatible_size <- function(group, index, expected_size, size, dots) {
  # called from the C++ code
  if(missing(dots)) {
    abort(class = "dplyr_summarise_incompatible_size", size = size, group = group, index = index, expected_size = expected_size)
  }

  # so that cnd_bullet_cur_group_label() correctly reports the faulty group
  peek_mask()$set_current_group(group)

  stop_dplyr(index, dots, "summarise", "must be recyclable",
    x = "`{error_name}` must be size {or_1(expected_size)}, not {size}.",
    i = "An earlier column had size {expected_size}."
  )
}


# arrange() ---------------------------------------------------------------

stop_arrange_transmute <- function(cnd) {
  error_name <- cnd$error_name

  if (!is.null(error_name)) {
    # error caught by mutate_cols()
    index <- sub("^.*_", "", error_name)
    error_expression <- cnd$error_expression

    bullets <- c(
      i = glue("Could not create a temporary column for `..{index}`."),
      i = glue("`..{index}` is `{error_expression}`.")
    )
  } else {
    bullets <- c(x = conditionMessage(cnd))
  }

  abort(c(
    "arrange() failed at implicit mutate() step. ",
    bullets
  ))

}
