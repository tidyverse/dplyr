arg_name <- function(quos, index) {
  name  <- names(quos)[index]
  if (is.null(name) || name == "") {
    name <- glue("..{index}")
  }
  name
}

cnd_bullet_cur_group_label <- function(what = "error") {
  label <- cur_group_label()
  if (label != "") {
    glue("The {what} occurred in {label}.")
  }
}

cnd_bullet_rowwise_unlist <- function() {
  data <- peek_mask()$full_data()
  if (inherits(data, "rowwise_df")) {
    glue_data(peek_error_context(), "Did you mean: `{error_name} = list({error_expression})` ?")
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

is_data_pronoun <- function(x) {
  is_call(x, c("[[", "$")) && identical(node_cadr(x), sym(".data"))
}

# Because as_label() strips off .data$<> and .data[[<>]]
quo_as_label <- function(quo)  {
  expr <- quo_get_expr(quo)
  if (is_data_pronoun(expr)) {
    deparse(expr)[[1]]
  } else{
    as_label(expr)
  }
}

local_error_context <- function(dots, .index, mask, frame = caller_env()) {
  expr <- dots[[.index]]
  if (quo_is_call(expr, "invisible")) {
    expr <- ""
  } else {
    expr <- quo_as_label(dots[[.index]])
  }

  error_context <- env(
    error_name = arg_name(dots, .index),
    error_expression = expr,
    mask = mask
  )
  context_local("dplyr_error_context", error_context, frame = frame)
}
peek_error_context <- function() {
  context_peek("dplyr_error_context", "peek_error_context", "dplyr error handling")
}

cnd_bullet_header <- function(what) {
  error_context <- peek_error_context()
  error_name <- error_context$error_name
  error_expression <- error_context$error_expression

  if (nzchar(error_expression)) {
    sep <- " = "
  } else {
    sep <- ""
  }

  glue("Problem while {what} `{error_name}{sep}{error_expression}`.")
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

dplyr_internal_error <- function(class = NULL, data = list()) {
  abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
}

skip_internal_condition <- function(cnd) {
  if (inherits(cnd, "dplyr:::internal_error")) {
    cnd$parent
  } else {
    cnd
  }
}
