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
  expr <- quo_squash(dots[[.index]])
  error_expression  <- paste(deparse(expr), collapse = "")

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

abort_glue <- function(message, .envir = parent.frame(), class = NULL) {
  if (length(message)) {
    message <- exec(glue, message, !!!.envir)
    exec(abort, message = message, class = class, !!!.envir)
  } else {
    exec(abort, class = class, !!!.envir)
  }
}

