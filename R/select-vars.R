#' Select variables.
#'
#' These functions power [select()] and [rename()].
#'
#' For historic reasons, the `vars` and `include` arguments are not
#' prefixed with `.`. This means that any argument starting with `v`
#' might partial-match on `vars` if it is not explicitly named. Also
#' `...` cannot accept arguments named `exclude` or `include`. You can
#' enquose and splice the dots to work around these limitations (see
#' examples).
#'
#' @param vars A character vector of existing column names.
#' @param ...,args Expressions to compute
#'
#'   These arguments are automatically [quoted][rlang::quo] and
#'   [evaluated][rlang::eval_tidy] in a context where elements of
#'   `vars` are objects representing their positions within
#'   `vars`. They support [unquoting][rlang::quasiquotation] and
#'   splicing. See `vignette("programming")` for an introduction to
#'   these concepts.
#'
#'   Note that except for `:`, `-` and `c()`, all complex expressions
#'   are evaluated outside that context. This is to prevent accidental
#'   matching to `vars` elements when you refer to variables from the
#'   calling context.
#' @param include,exclude Character vector of column names to always
#'   include/exclude.
#' @seealso [select_var()]
#' @export
#' @keywords internal
#' @return A named character vector. Values are existing column names,
#'   names are new names.
#' @examples
#' # Keep variables
#' select_vars(names(iris), everything())
#' select_vars(names(iris), starts_with("Petal"))
#' select_vars(names(iris), ends_with("Width"))
#' select_vars(names(iris), contains("etal"))
#' select_vars(names(iris), matches(".t."))
#' select_vars(names(iris), Petal.Length, Petal.Width)
#' select_vars(names(iris), one_of("Petal.Length", "Petal.Width"))
#'
#' df <- as.data.frame(matrix(runif(100), nrow = 10))
#' df <- df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)]
#' select_vars(names(df), num_range("V", 4:6))
#'
#' # Drop variables
#' select_vars(names(iris), -starts_with("Petal"))
#' select_vars(names(iris), -ends_with("Width"))
#' select_vars(names(iris), -contains("etal"))
#' select_vars(names(iris), -matches(".t."))
#' select_vars(names(iris), -Petal.Length, -Petal.Width)
#'
#' # Rename variables
#' select_vars(names(iris), petal_length = Petal.Length)
#' select_vars(names(iris), petal = starts_with("Petal"))
#'
#' # Rename variables preserving all existing
#' rename_vars(names(iris), petal_length = Petal.Length)
#'
#' # You can unquote names or formulas (or lists of)
#' select_vars(names(iris), !!! list(quo(Petal.Length)))
#' select_vars(names(iris), !! quote(Petal.Length))
#'
#' # The .data pronoun is available:
#' select_vars(names(mtcars), .data$cyl)
#' select_vars(names(mtcars), .data$mpg : .data$disp)
#'
#' # However it isn't available within calls since those are evaluated
#' # outside of the data context. This would fail if run:
#' # select_vars(names(mtcars), identical(.data$cyl))
#'
#'
#' # If you're writing a wrapper around select_vars(), pass the dots
#' # via splicing to avoid matching dotted arguments to select_vars()
#' # named arguments (`vars`, `include` and `exclude`):
#' wrapper <- function(...) {
#'   select_vars(names(mtcars), !!! quos(...))
#' }
#'
#' # This won't partial-match on `vars`:
#' wrapper(var = cyl)
#'
#' # This won't match on `include`:
#' wrapper(include = cyl)
select_vars <- function(vars, ..., include = character(), exclude = character()) {
  quos <- quos(...)

  if (is_empty(quos)) {
    vars <- setdiff(include, exclude)
    return(set_names(vars, vars))
  }

  # Set current_vars so available to select_helpers
  old <- set_current_vars(vars)
  on.exit(set_current_vars(old), add = TRUE)

  # Map variable names to their positions: this keeps integer semantics
  names_list <- set_names(as.list(seq_along(vars)), vars)

  # if the first selector is exclusive (negative), start with all columns
  first <- f_rhs(quos[[1]])
  initial_case <- if (is_negated(first)) list(seq_along(vars)) else integer(0)

  # Evaluate symbols in an environment where columns are bound, but
  # not calls (select helpers are scoped in the calling environment)
  is_helper <- map_lgl(quos, quo_is_helper)
  ind_list <- map_if(quos, is_helper, eval_tidy)
  ind_list <- map_if(ind_list, !is_helper, eval_tidy, data = names_list)

  ind_list <- c(initial_case, ind_list)
  names(ind_list) <- c(names2(initial_case), names2(quos))

  # Match strings to variable positions
  ind_list <- map_if(ind_list, is_character, match_var, table = vars)

  is_integerish <- map_lgl(ind_list, is_integerish)
  if (any(!is_integerish)) {
    bad <- quos[!is_integerish]
    first <- ind_list[!is_integerish][[1]]
    first_type <- friendly_type(type_of(first))
    bad_calls(bad,
      "must resolve to integer column positions, not {first_type}"
    )
  }

  incl <- combine_vars(vars, ind_list)

  # Include/exclude specified variables
  sel <- set_names(vars[incl], names(incl))
  sel <- c(setdiff2(include, sel), sel)
  sel <- setdiff2(sel, exclude)

  # Ensure all output vars named
  if (is_empty(sel)) {
    names(sel) <- sel
  } else {
    unnamed <- names2(sel) == ""
    names(sel)[unnamed] <- sel[unnamed]
  }

  sel
}

quo_is_helper <- function(quo) {
  expr <- f_rhs(quo)

  if (!is_lang(expr)) {
    return(FALSE)
  }

  if (is_data_pronoun(expr)) {
    return(FALSE)
  }

  if (is_lang(expr, c("-", ":", "c"))) {
    return(FALSE)
  }

  TRUE
}
match_var <- function(chr, table) {
  pos <- match(chr, table)
  if (any(are_na(pos))) {
    chr <- glue::collapse(chr[are_na(pos)], ", ")
    abort(glue("Strings must match column names. Unknown columns: {chr}"))
  }
  pos
}

#' @rdname se-deprecated
#' @inheritParams select_vars
#' @export
select_vars_ <- function(vars, args, include = character(), exclude = character()) {
  args <- compat_lazy_dots(args, caller_env())
  select_vars(vars, !!! args, include = include, exclude = exclude)
}

setdiff2 <- function(x, y) {
  x[match(x, y, 0L) == 0L]
}

#' @export
#' @rdname select_vars
#' @param strict If `TRUE`, will throw an error if you attempt to rename a
#'   variable that doesn't exist.
rename_vars <- function(vars, ..., strict = TRUE) {
  exprs <- exprs(...)
  if (any(names2(exprs) == "")) {
    abort("All arguments must be named")
  }

  old_vars <- map2(exprs, names(exprs), switch_rename)
  new_vars <- names(exprs)

  unknown_vars <- setdiff(old_vars, vars)
  if (strict && length(unknown_vars) > 0) {
    bad_args(unknown_vars, "contains unknown variables")
  }

  select <- set_names(vars, vars)
  names(select)[match(old_vars, vars)] <- new_vars

  select
}
#' @export
#' @rdname se-deprecated
rename_vars_ <- function(vars, args) {
  args <- compat_lazy_dots(args, caller_env())
  rename_vars(vars, !!! args)
}

# FIXME: that's not a tidy implementation yet because we need to
# handle non-existing symbols silently when `strict = FALSE`
switch_rename <- function(expr, name) {
  switch_type(expr,
    string = ,
    symbol =
      return(as_string(expr)),
    language =
      if (is_data_pronoun(expr)) {
        args <- node_cdr(expr)
        return(switch_rename(node_cadr(args)))
      } else {
        abort("Expressions are currently not supported in `rename()`")
      }
  )

  actual_type <- friendly_type(type_of(expr))
  named_call <- ll(!! name := expr)
  bad_named_calls(named_call, "must be a symbol or a string, not {actual_type}")
}
