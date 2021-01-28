#' Apply a function (or functions) across multiple columns
#'
#' @description
#' `across()` makes it easy to apply the same transformation to multiple
#' columns, allowing you to use [select()] semantics inside in "data-masking"
#' functions like [summarise()] and [mutate()]. See `vignette("colwise")` for
#'  more details.
#'
#' `if_any()` and `if_all()` are used with `filter()` to apply the same
#' predicate function to a selection of columns and combine the
#' results into a single logical vector.
#'
#' `across()` supersedes the family of "scoped variants" like
#' `summarise_at()`, `summarise_if()`, and `summarise_all()`.
#'
#' @param cols,.cols <[`tidy-select`][dplyr_tidy_select]> Columns to transform.
#'   Because `across()` is used within functions like `summarise()` and
#'   `mutate()`, you can't select or compute upon grouping variables.
#' @param .fns Functions to apply to each of the selected columns.
#'   Possible values are:
#'
#'   - `NULL`, to returns the columns untransformed.
#'   - A function, e.g. `mean`.
#'   - A purrr-style lambda, e.g. `~ mean(.x, na.rm = TRUE)`
#'   - A list of functions/lambdas, e.g.
#'     `list(mean = mean, n_miss = ~ sum(is.na(.x))`
#'
#'   Within these functions you can use [cur_column()] and [cur_group()]
#'   to access the current column and grouping keys respectively.
#' @param ... Additional arguments for the function calls in `.fns`.
#' @param .names A glue specification that describes how to name the output
#'   columns. This can use `{.col}` to stand for the selected column name, and
#'   `{.fn}` to stand for the name of the function being applied. The default
#'   (`NULL`) is equivalent to `"{.col}"` for the single function case and
#'   `"{.col}_{.fn}"` for the case where a list is used for `.fns`.
#'
#' @returns
#' `across()` returns a tibble with one column for each column in `.cols` and each function in `.fns`.
#'
#' `if_any()` and `if_all()` return a logical vector.
#' @examples
#' # across() -----------------------------------------------------------------
#' # Different ways to select the same set of columns
#' # See <https://tidyselect.r-lib.org/articles/syntax.html> for details
#' iris %>%
#'   as_tibble() %>%
#'   mutate(across(c(Sepal.Length, Sepal.Width), round))
#' iris %>%
#'   as_tibble() %>%
#'   mutate(across(c(1, 2), round))
#' iris %>%
#'   as_tibble() %>%
#'   mutate(across(1:Sepal.Width, round))
#' iris %>%
#'   as_tibble() %>%
#'   mutate(across(where(is.double) & !c(Petal.Length, Petal.Width), round))
#'
#' # A purrr-style formula
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), ~mean(.x, na.rm = TRUE)))
#'
#' # A named list of functions
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd)))
#'
#' # Use the .names argument to control the output names
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean, .names = "mean_{.col}"))
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean = mean, sd = sd), .names = "{.col}.{.fn}"))
#'
#' # When the list is not named, .fn is replaced by the function's position
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), list(mean, sd), .names = "{.col}.fn{.fn}"))
#'
#' # if_any() and if_all() ----------------------------------------------------
#' iris %>%
#'   filter(if_any(ends_with("Width"), ~ . > 4))
#' iris %>%
#'   filter(if_all(ends_with("Width"), ~ . > 2))
#'
#' @export
#' @seealso [c_across()] for a function that returns a vector
across <- function(.cols = everything(), .fns = NULL, ..., .names = NULL) {
  key <- key_deparse(sys.call())
  setup <- across_setup({{ .cols }}, fns = .fns, names = .names, key = key, .caller_env = caller_env())

  vars <- setup$vars
  if (length(vars) == 0L) {
    return(new_tibble(list(), nrow = 1L))
  }
  fns <- setup$fns
  names <- setup$names

  mask <- peek_mask()
  data <- mask$current_cols(vars)

  if (is.null(fns)) {
    nrow <- length(mask$current_rows())
    data <- new_data_frame(data, n = nrow, class = c("tbl_df", "tbl"))

    if (is.null(names)) {
      return(data)
    } else {
      return(set_names(data, names))
    }
  }

  n_cols <- length(data)
  n_fns <- length(fns)

  seq_n_cols <- seq_len(n_cols)
  seq_fns <- seq_len(n_fns)

  k <- 1L
  out <- vector("list", n_cols * n_fns)

  # Reset `cur_column()` info on exit
  old_var <- context_peek_bare("column")
  on.exit(context_poke("column", old_var), add = TRUE)

  # Loop in such an order that all functions are applied
  # to a single column before moving on to the next column
  for (i in seq_n_cols) {
    var <- vars[[i]]
    col <- data[[i]]

    context_poke("column", var)

    for (j in seq_fns) {
      fn <- fns[[j]]
      out[[k]] <- fn(col, ...)
      k <- k + 1L
    }
  }

  size <- vec_size_common(!!!out)
  out <- vec_recycle_common(!!!out, .size = size)
  names(out) <- names
  new_data_frame(out, n = size, class = c("tbl_df", "tbl"))
}

#' @rdname across
#' @export
if_any <- function(.cols, .fns = NULL, ..., .names = NULL) {
  df <- across({{.cols}}, .fns = .fns, ..., .names = .names)
  .Call(dplyr_reduce_lgl_or, df)
}

#' @rdname across
#' @export
if_all <- function(.cols, .fns = NULL, ..., .names = NULL) {
  df <- across({{.cols}}, .fns = .fns, ..., .names = .names)
  .Call(dplyr_reduce_lgl_and, df)
}

#' Combine values from multiple columns
#'
#' @description
#' `c_across()` is designed to work with [rowwise()] to make it easy to
#' perform row-wise aggregations. It has two differences from `c()`:
#'
#' * It uses tidy select semantics so you can easily select multiple variables.
#'   See `vignette("rowwise")` for more details.
#'
#' * It uses [vctrs::vec_c()] in order to give safer outputs.
#'
#' @inheritParams across
#' @seealso [across()] for a function that returns a tibble.
#' @export
#' @examples
#' df <- tibble(id = 1:4, w = runif(4), x = runif(4), y = runif(4), z = runif(4))
#' df %>%
#'   rowwise() %>%
#'   mutate(
#'     sum = sum(c_across(w:z)),
#'     sd = sd(c_across(w:z))
#'  )
c_across <- function(cols = everything()) {
  key <- key_deparse(sys.call())
  vars <- c_across_setup({{ cols }}, key = key)

  mask <- peek_mask("c_across()")

  cols <- mask$current_cols(vars)
  vec_c(!!!cols, .name_spec = zap())
}

across_glue_mask <- function(.col, .fn, .caller_env) {
  glue_mask <- env(.caller_env, .col = .col, .fn = .fn)
  # TODO: we can make these bindings louder later
  env_bind_active(
    glue_mask, col = function() glue_mask$.col, fn = function() glue_mask$.fn
  )
  glue_mask
}

# TODO: The usage of a cache in `across_setup()` and `c_across_setup()` is a stopgap solution, and
# this idea should not be used anywhere else. This should be replaced by the
# next version of hybrid evaluation, which should offer a way for any function
# to do any required "set up" work (like the `eval_select()` call) a single
# time per top-level call, rather than once per group.
across_setup <- function(cols, fns, names, key, .caller_env) {
  mask <- peek_mask("across()")
  value <- mask$across_cache_get(key)
  if (is.null(value)) {
    value <- across_setup_impl({{cols}},
      fns = fns, names = names, .caller_env = .caller_env, mask = mask
    )
    mask$across_cache_add(key, value)
  }
  value
}

across_setup_impl <- function(cols, fns, names, .caller_env, mask = peek_mask("across()")) {
  cols <- enquo(cols)

  is_top_across <- mask$get_current_group() == 0L
  if (is_top_across) {
    # FIXME: this is a little bit hacky to make top_across()
    #        work, otherwise mask$across_cols() fails when calling
    #        self$current_cols(across_vars_used)
    #        it should not affect anything because it is expected that
    #        across_setup() is only ever called on the first group anyway
    #        but perhaps it is time to review how across_cols() work
    mask$set_current_group(1L)
  } else {
    # The real `across()` is evaluated in a data mask so we need to remove the
    # mask layer from the quosure environment (#5460)
    cols <- quo_set_env(cols, data_mask_top(quo_get_env(cols), recursive = FALSE, inherit = TRUE))
  }

  vars <- tidyselect::eval_select(cols, data = mask$across_cols())
  vars <- names(vars)

  if (is.null(fns)) {
    if (!is.null(names)) {
      glue_mask <- across_glue_mask(.caller_env, .col = vars, .fn = "1")
      names <- vec_as_names(glue(names, .envir = glue_mask), repair = "check_unique")
    }

    value <- list(vars = vars, fns = fns, names = names)
    return(value)
  }

  # apply `.names` smart default
  if (is.function(fns) || is_formula(fns)) {
    names <- names %||% "{.col}"
    fns <- list("1" = fns)
  } else {
    names <- names %||% "{.col}_{.fn}"
  }

  if (!is.list(fns)) {
    abort(c("Problem with `across()` input `.fns`.",
      i = "Input `.fns` must be NULL, a function, a formula, or a list of functions/formulas."
    ))
  }

  fns <- map(fns, function(fn) {
    if (is_formula(fn)) {
      f_env(fn) <- mask$get_env_bindings()
    }
    as_function(fn)
  })

  # make sure fns has names, use number to replace unnamed
  if (is.null(names(fns))) {
    names_fns <- seq_along(fns)
  } else {
    names_fns <- names(fns)
    empties <- which(names_fns == "")
    if (length(empties)) {
      names_fns[empties] <- empties
    }
  }

  glue_mask <- glue_mask <- across_glue_mask(.caller_env,
    .col = rep(vars, each = length(fns)),
    .fn  = rep(names_fns, length(vars))
  )
  names <- vec_as_names(glue(names, .envir = glue_mask), repair = "check_unique")

  list(vars = vars, fns = fns, names = names)
}

# FIXME: This pattern should be encapsulated by rlang
data_mask_top <- function(env, recursive = FALSE, inherit = FALSE) {
  while (env_has(env, ".__tidyeval_data_mask__.", inherit = inherit)) {
    env <- env_parent(env_get(env, ".top_env", inherit = inherit))
    if (!recursive) {
      return(env)
    }
  }

  env
}

c_across_setup <- function(cols, key) {
  mask <- peek_mask("c_across()")

  value <- mask$across_cache_get(key)
  if (!is.null(value)) {
    return(value)
  }

  cols <- enquo(cols)
  across_cols <- mask$across_cols()

  vars <- tidyselect::eval_select(expr(!!cols), across_cols)
  value <- names(vars)

  mask$across_cache_add(key, value)

  value
}

key_deparse <- function(key) {
  deparse(key, width.cutoff = 500L, backtick = TRUE, nlines = 1L, control = NULL)
}

# When mutate() or summarise() have an unnamed call to across() at the top level, e.g.
# summarise(across(<...>)) or mutate(across(<...>))
#
# a call to top_across(<...>) is evaluated instead.
# top_across() returns a flattened list of expressions along with some
# information about the "current column" for each expression
# in the "columns" attribute:
#
# For example with: summarise(across(c(x, y), mean, .names = "mean_{.col}")) top_across() will return
# something like:
#
# structure(
#   list(mean_x = expr(mean(x)), mean_y = expr(mean(y)))
#   columns = c("x", "y")
# )
top_across <- function(.cols = everything(), .fns = NULL, ..., .names = NULL) {
  setup <- across_setup_impl({{ .cols }}, fns = .fns, names = .names, .caller_env = caller_env())
  vars <- setup$vars

  # nothing
  if (length(vars) == 0L) {
    return(list())
  }

  fns <- setup$fns
  names <- setup$names

  # no functions, so just return a list of symbols
  if (is.null(fns)) {
    expressions <- syms(vars)
    names(expressions) <- if (is.null(.names)) vars else names
    return(expressions)
  }

  n_vars <- length(vars)
  n_fns <- length(fns)

  seq_vars <- seq_len(n_vars)
  seq_fns  <- seq_len(n_fns)

  expressions <- vector(mode = "list", n_vars * n_fns)
  columns <- character(n_vars * n_fns)
  extra_args <- list(...)

  k <- 1L
  for (i in seq_vars) {
    var <- vars[[i]]

    for (j in seq_fns) {
      fn <- fns[[j]]
      call <- call2(fn, sym(var), !!!extra_args)
      expressions[[k]] <- call
      columns[[k]] <- var
      k <- k + 1L
    }
  }
  names(expressions) <- names
  attr(expressions, "columns") <- columns
  expressions
}

new_dplyr_quosure <- function(quo, ...) {
  attr(quo, "dplyr:::data") <- list2(...)
  quo
}

dplyr_quosures <- function(...) {
  quosures <- enquos(..., .ignore_empty = "all")
  names_given <- names2(quosures)
  names_auto  <- names(enquos(..., .named = TRUE, .ignore_empty = "all"))

  for (i in seq_along(quosures)) {
    quosures[[i]] <- new_dplyr_quosure(quosures[[i]],
      name_given = names_given[i],
      name_auto = names_auto[i],
      is_named = names_given[i] != "",
      index = i
    )
  }
  quosures
}

expand_quosure <- function(quo) {
  quo_data <- attr(quo, "dplyr:::data")
  if (quo_is_call(quo, "across", ns = c("", "dplyr")) && !quo_data$is_named) {
    # call top_across() instead of across()
    quo_env <- quo_get_env(quo)
    quo <- new_quosure(node_poke_car(quo_get_expr(quo), top_across), quo_env)
    expressions <- eval_tidy(quo)
    names_expressions <- names(expressions)

    # process the results of top_across()
    quosures <- vector(mode = "list", length(expressions))
    for (j in seq_along(expressions)) {
      name <- names_expressions[j]
      quosures[[j]] <- new_dplyr_quosure(new_quosure(expressions[[j]], quo_env),
        name_given = name,
        name_auto = name,
        is_named = TRUE,
        index = c(quo_data$index, j),
        column = attr(expressions, "columns")[j]
      )
    }
  } else {
    quosures <- list(quo)
  }

  quosures
}
