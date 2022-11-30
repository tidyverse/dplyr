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

cnd_bullet_cur_group_label <- function(what = "error") {
  label <- cur_group_label()
  if (label != "") {
    glue("In {label}.")
  }
}

cnd_bullet_rowwise_unlist <- function() {
  if (peek_mask()$is_rowwise()) {
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

has_active_group_context <- function(mask) {
  mask$get_current_group() != 0L
}

# Common ------------------------------------------------------------------

is_data_pronoun <- function(x) {
  is_call(x, c("[[", "$")) && identical(x[[2]], sym(".data"))
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

local_error_context <- function(dots, i, mask, frame = caller_env()) {
  ctxt <- new_error_context(dots, i, mask = mask)
  context_local("dplyr_error_context", ctxt, frame = frame)
}
peek_error_context <- function() {
  context_peek("dplyr_error_context", "dplyr error handling")
}

new_error_context <- function(dots, i, mask) {
  if (!length(dots) || i == 0L) {
    env(
      error_name = "",
      error_expression = NULL,
      mask = mask
    )
  } else {
    expr <- quo_as_label(dots[[i]])

    env(
      error_name = names(dots)[i],
      error_expression = expr,
      mask = mask
    )
  }
}

# Doesn't restore values. To be called within a
# `local_error_context()` in charge of restoring.
poke_error_context <- function(dots, i, mask) {
  ctxt <- new_error_context(dots, i, mask = mask)
  context_poke("dplyr_error_context", ctxt)
}

mask_type <- function(mask = peek_mask()) {
  if (mask$get_size() > 0) {
    if (mask$is_grouped()) {
      return("grouped")
    } else if (mask$is_rowwise()) {
      return("rowwise")
    }
  }
  "ungrouped"
}

ctxt_error_label <- function(ctxt = peek_error_context()) {
  error_label(ctxt$error_name, ctxt$error_expression)
}
error_label <- function(name, expr_label) {
  if (is_null(name) || !nzchar(name)) {
    expr_label
  } else {
    name
  }
}

ctxt_error_label_named <- function(ctxt = peek_error_context()) {
  error_label_named(ctxt$error_name, ctxt$error_expression)
}
error_label_named <- function(name, expr_label) {
  if (is_null(name) || !nzchar(name)) {
    expr_label
  } else {
    paste0(name, " = ", expr_label)
  }
}

cnd_bullet_header <- function(what) {
  ctxt <- peek_error_context()
  label <- ctxt_error_label_named(ctxt)

  if (is_string(what, "recycle")) {
    glue("Can't {what} `{label}`.")
  } else {
    c("i" = glue("In argument: `{label}`."))
  }
}

cnd_bullet_combine_details <- function(x, arg) {
  id <- as.integer(sub("^..", "", arg))
  group <- peek_mask()$get_keys()[id, ]
  details <- cur_group_label(id = group, group = group)
  glue("Result of type <{vec_ptype_full(x)}> for {details}.")
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

dplyr_error_handler <- function(dots,
                                mask,
                                bullets,
                                error_call,
                                action = "compute",
                                error_class = NULL,
                                i_sym = "i",
                                frame = caller_env()) {
  force(frame)

  function(cnd) {
    local_error_context(dots, i = frame[[i_sym]], mask = mask)

    if (inherits(cnd, "dplyr:::internal_error")) {
      parent <- error_cnd(message = bullets(cnd))
    } else {
      parent <- cnd
    }

    # FIXME: Must be after calling `bullets()` because the
    # `dplyr:::summarise_incompatible_size` method sets the correct
    # group by side effect
    message <- c(
      cnd_bullet_header(action),
      "i" = if (has_active_group_context(mask)) cnd_bullet_cur_group_label()
    )

    abort(
      message,
      class = error_class,
      parent = parent,
      call = error_call
    )
  }
}


# Warnings -------------------------------------------------------------

#' Show warnings from the last command
#'
#' Warnings that occur inside a dplyr verb like `mutate()` are caught
#' and stashed away instead of being emitted to the console. This
#' prevents rowwise and grouped data frames from flooding the console
#' with warnings. To see the original warnings, use
#' `last_dplyr_warnings()`.
#'
#' @param n Passed to [head()] so that only the first `n` warnings are
#'   displayed.
#' @keywords internal
#' @export
last_dplyr_warnings <- function(n = 5) {
  if (!identical(n, Inf)) {
    check_number_whole(n)
    stopifnot(n >= 0)
  }

  warnings <- the$last_warnings
  n_remaining <- max(length(warnings) - n, 0L)

  warnings <- head(warnings, n = n)
  warnings <- map(warnings, new_dplyr_warning)

  structure(
    warnings,
    class = c("last_dplyr_warnings", "list"),
    n_shown = n,
    n_remaining = n_remaining
  )
}

on_load({
  the$last_warnings <- list()
  the$last_cmd_frame <- ""
})

dplyr_warning_handler <- function(state, mask, error_call) {
  # `error_call()` does some non-trivial work, e.g. climbing frame
  # environments to find generic calls. We avoid evaluating it
  # repeatedly in the loop by assigning it here (lazily as we only
  # need it for the error path).
  delayedAssign("error_call_forced", error_call(error_call))

  function(cnd) {
    # Don't entrace more than 5 warnings because this is very costly
    if (is_null(cnd$trace) && length(state$warnings) < 5) {
      cnd$trace <- trace_back(bottom = error_call)
    }

    new <- cnd_data(
      cnd = cnd,
      ctxt = peek_error_context(),
      mask = mask,
      call = error_call_forced
    )

    state$warnings <- c(state$warnings, list(new))
    maybe_restart("muffleWarning")
  }
}

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

signal_warnings <- function(state, error_call) {
  warnings <- state$warnings

  n <- length(warnings)
  if (!n) {
    return()
  }

  push_dplyr_warnings(warnings)

  first <- new_dplyr_warning(warnings[[1]])
  call <- format_error_call(error_call)

  if (nzchar(names2(cnd_header(first))[[1]])) {
    prefix <- NULL
  } else {
    prefix <- paste0(cli::col_yellow("!"), " ")
  }

  msg <- paste_line(
    cli::format_warning(c(
      "There {cli::qty(n)} {?was/were} {n} warning{?s} in {call}.",
      if (n > 1) "The first warning was:"
    )),
    paste0(prefix, cnd_message(first)),
    if (n > 1) cli::format_warning(c(
      i = "Run {.run dplyr::last_dplyr_warnings()} to see the {n - 1} remaining warning{?s}."
    ))
  )

  warn(msg, use_cli_format = FALSE)
}

new_dplyr_warning <- function(data) {
  if (data$has_group_data) {
    group_label <- cur_group_label(
      data$type,
      data$group_data$id,
      data$group_data$group
    )
  } else {
    group_label <- ""
  }

  label <- error_label_named(data$name, data$expr)

  msg <- c(
    "i" = glue::glue("In argument: `{label}`."),
    "i" = if (nzchar(group_label)) glue("In {group_label}.")
  )

  warning_cnd(
    message = msg,
    parent = data$cnd,
    call = data$call,
    trace = data$cnd$trace
  )
}

#' @export
print.last_dplyr_warnings <- function(x, ...) {
  # Opt into experimental grayed out tree
  local_options(
    "rlang:::trace_display_tree" = TRUE
  )
  print(unstructure(x), ..., simplify = "none")

  n_remaining <- attr(x, "n_remaining")
  if (n_remaining) {
    n_more <- attr(x, "n_shown") * 2
    cli::cli_bullets(c(
      "... with {n_remaining} more warning{?s}.",
      "i" = "Run {.run dplyr::last_dplyr_warnings(n = {n_more})} to show more."
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

cnd_message_lines <- function(cnd, ...) {
  c(
    "!" = cnd_header(cnd, ...),
    cnd_body(cnd, ...),
    cnd_footer(cnd, ...)
  )
}
