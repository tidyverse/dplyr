#' Local error call for dplyr verbs
#' @noRd
dplyr_local_error_call <- function(call = frame, frame = caller_env()) {
  # This doesn't implement the semantics of a `local_` function
  # perfectly in order to be as fast as possible
  frame$.__dplyr_error_call__. <- call
  invisible(NULL)
}

# Takes the local call by default. If the caller of the verb has
# called `dplyr_local_error_call()`, we used that call instead.  This
# logic is slightly different than in checking functions or error
# helpers, where the error call is always taken from the parent by
# default.
dplyr_error_call <- function(call) {
  if (is_missing(call)) {
    call <- caller_env()
  }

  while (is_environment(call)) {
    caller <- eval_bare(quote(base::parent.frame()), call)
    caller_call <- caller[[".__dplyr_error_call__."]]

    if (is_null(caller_call)) {
      break
    }

    call <- caller_call
  }

  call
}

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
  if (peek_mask()$is_rowwise_df()) {
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
  expr <- dots_expr(dots, .index)

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

dots_expr <- function(dots, i) {
 expr <- dots[[i]]
  if (quo_is_call(expr, "invisible")) {
    ""
  } else {
    quo_as_label(dots[[i]])
  }
}

mask_type <- function(mask = peek_mask()) {
  if (mask$get_size() > 0) {
    if (mask$is_grouped_df()) {
      return("grouped")
    } else if (mask$is_rowwise_df()) {
      return("rowwise")
    }
  }
  "ungrouped"
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
  keys <- peek_mask()$get_keys()[group, ]
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

err_locs <- function(x) {
  if (!is.integer(x)) {
    abort("`x` must be an integer vector of locations.", .internal = TRUE)
  }

  size <- length(x)

  if (size == 0L) {
    abort("`x` must have at least 1 location.", .internal = TRUE)
  }

  if (size > 5L) {
    x <- x[1:5]
    extra <- glue(" and {size - 5L} more")
  } else {
    extra <- ""
  }

  x <- glue_collapse(x, sep = ", ")

  glue("`c({x})`{extra}")
}

dplyr_internal_error <- function(class = NULL, data = list()) {
  abort(class = c(class, "dplyr:::internal_error"), dplyr_error_data = data)
}
dplyr_internal_signal <- function(class) {
  signal(message = "Internal dplyr signal", class = c(class, "dplyr:::internal_signal"))
}

skip_internal_condition <- function(cnd) {
  if (inherits(cnd, "dplyr:::internal_error")) {
    cnd$parent
  } else {
    cnd
  }
}


# Warnings -------------------------------------------------------------

#' Show warnings from the last command
#'
#' Warnings that occur inside a dplyr verb like `mutate()` are caught
#' and stashed away instead of being emitted to the console. This
#' prevents rowwise and grouped data frames from flooding the console
#' with warnings. To see the original warnings, use
#' `dplyr_last_warnings()`.
#'
#' @param n Passed to [head()] so that only the first `n` warnings are
#'   displayed.
#' @export
dplyr_last_warnings <- function(n = 20) {
  if (!identical(n, Inf)) {
    check_number(n)
    stopifnot(n >= 0)
  }

  warnings <- the$last_warnings
  remaining <- max(length(warnings) - n, 0L)

  warnings <- head(warnings, n = n)
  warnings <- map(warnings, new_dplyr_warning)

  structure(
    warnings,
    class = c("dplyr_last_warnings", "list"),
    remaining = remaining
  )
}

the$last_warnings <- list()
the$last_cmd_frame <- ""

# Flushes warnings if a new top-level command is detected
push_dplyr_warnings <- function(warnings) {
  last <- the$last_cmd_frame
  current <- obj_address(sys.frame(1))

  if (!identical(last, current)) {
    reset_dplyr_warnings()
    the$last_cmd_frame <- current
  }

  the$last_warnings <- c(the$last_warnings, warnings)
}

# Also used in tests
reset_dplyr_warnings <- function() {
  the$last_warnings <- list()
}

new_dplyr_warning <- function(data) {
  label <- cur_group_label(
    data$type,
    data$group_data$id,
    data$group_data$group
  )
  if (nzchar(label)) {
    label <- glue(" in {label}")
  }

  msg <- glue::glue("Problem{label} while computing `{data$name} = {data$expr}`.")

  warning_cnd(
    message = msg,
    parent = data$cnd,
    call = data$call
  )
}

#' @export
print.dplyr_last_warnings <- function(x, ...) {
  print(unstructure(x), ...)

  remaining <- attr(x, "remaining")
  if (remaining) {
    cli::cli_bullets(c(
      "... with {remaining} more warning{?s}.",
      "i" = "Use {.code dplyr_last_warnings(n = ...)} to show more."
    ))
  }
}

# rlang should export this routine
error_call <- function(call) {
  tryCatch(
    abort("", call = call),
    error = conditionCall
  )
}
