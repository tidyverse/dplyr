#' Group by one or more variables
#'
#' @description
#' Most data operations are done on groups defined by variables.
#' `group_by()` takes an existing tbl and converts it into a grouped tbl
#' where operations are performed "by group". `ungroup()` removes grouping.
#'
#' @family grouping functions
#' @inheritParams arrange
#' @param ... In `group_by()`, variables or computations to group by.
#'   In `ungroup()`, variables to remove from the grouping.
#' @param .add When `FALSE`, the default, `group_by()` will
#'   override existing groups. To add to the existing groups, use
#'   `.add = TRUE`.
#'
#'   This argument was previously called `add`, but that prevented
#'   creating a new grouping variable called `add`, and conflicts with
#'   our naming conventions.
#' @param .drop When `.drop = TRUE`, empty groups are dropped. See [group_by_drop_default()] for
#'   what the default value is for this argument.
#' @return A grouped data frame with class [`grouped_df`][grouped_df],
#'   unless the combination of `...` and `add` yields a empty set of
#'   grouping columns, in which case a tibble will be returned.
#' @section Methods:
#' These function are **generic**s, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' Methods available in currently loaded packages:
#'
#' * `group_by()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("group_by")}.
#' * `ungroup()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("ungroup")}.
#' @export
#' @examples
#' by_cyl <- mtcars %>% group_by(cyl)
#'
#' # grouping doesn't change how the data looks (apart from listing
#' # how it's grouped):
#' by_cyl
#'
#' # It changes how it acts with the other dplyr verbs:
#' by_cyl %>% summarise(
#'   disp = mean(disp),
#'   hp = mean(hp)
#' )
#' by_cyl %>% filter(disp == max(disp))
#'
#' # Each call to summarise() removes a layer of grouping
#' by_vs_am <- mtcars %>% group_by(vs, am)
#' by_vs <- by_vs_am %>% summarise(n = n())
#' by_vs
#' by_vs %>% summarise(n = sum(n))
#'
#' # To removing grouping, use ungroup
#' by_vs %>%
#'   ungroup() %>%
#'   summarise(n = sum(n))
#'
#' # You can group by expressions: this is just short-hand for
#' # a mutate() followed by a group_by()
#' mtcars %>% group_by(vsam = vs + am)
#'
#' # By default, group_by() overrides existing grouping
#' by_cyl %>%
#'   group_by(vs, am) %>%
#'   group_vars()
#'
#' # Use add = TRUE to instead append
#' by_cyl %>%
#'   group_by(vs, am, .add = TRUE) %>%
#'   group_vars()
#'
#'
#' # when factors are involved, groups can be empty
#' tbl <- tibble(
#'   x = 1:10,
#'   y = factor(rep(c("a", "c"), each  = 5), levels = c("a", "b", "c"))
#' )
#' tbl %>%
#'   group_by(y) %>%
#'   group_rows()
#'
group_by <- function(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data)) {
  UseMethod("group_by")
}

#' @export
group_by.data.frame <- function(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data)) {
  groups <- group_by_prepare(.data, ..., .add = .add)
  grouped_df(groups$data, groups$group_names, .drop)
}

#' @rdname group_by
#' @export
#' @param x A [tbl()]
ungroup <- function(x, ...) {
  UseMethod("ungroup")
}

#' @export
ungroup.grouped_df <- function(x, ...) {
  if (missing(...)) {
    as_tibble(x)
  } else {
    old_groups <- group_vars(x)
    to_remove <- tidyselect::vars_select(names(x), ...)

    new_groups <- setdiff(old_groups, to_remove)
    group_by(x, !!!syms(new_groups))
  }
}

#' @export
ungroup.rowwise_df <- function(x, ...) {
  ellipsis::check_dots_empty()
  as_tibble(x)
}

#' @export
ungroup.data.frame <- function(x, ...) {
  ellipsis::check_dots_empty()
  x
}

#' Prepare for grouping.
#'
#' `*_prepare()` performs standard manipulation that is needed prior
#' to actual data processing. They are only be needed by packages
#' that implement dplyr backends.
#'
#' @return A list
#'   \item{data}{Modified tbl}
#'   \item{groups}{Modified groups}
#' @export
#' @keywords internal
group_by_prepare <- function(.data, ..., .add = FALSE, .dots = deprecated(), add = deprecated()) {

  if (!missing(add)) {
    lifecycle::deprecate_warn("1.0.0", "group_by(add = )", "group_by(.add = )")
    .add <- add
  }

  new_groups <- enquos(...)
  if (!missing(.dots)) {
    # Used by dbplyr 1.4.2 so can't aggressively deprecate
    lifecycle::deprecate_soft("1.0.0", "group_by(.dots = )")
    new_groups <- c(new_groups, compat_lazy_dots(.dots, env = caller_env()))
  }
  new_groups <- new_groups[!map_lgl(new_groups, quo_is_missing)]

  # If any calls, use mutate to add new columns, then group by those
  computed_columns <- add_computed_columns(.data, new_groups)
  out <- computed_columns$data
  group_names <- computed_columns$added_names

  if (.add) {
    group_names <- union(group_vars(.data), group_names)
  }

  unknown <- setdiff(group_names, tbl_vars(out))
  if (length(unknown) > 0) {
    abort(c(
      "Must group by variables found in `.data`.",
      glue("Column `{unknown}` is not found.")
    ))
  }

  list(
    data = out,
    groups = syms(group_names),
    group_names = group_names
  )
}

add_computed_columns <- function(.data, vars) {
  is_symbol <- map_lgl(vars, quo_is_variable_reference)
  needs_mutate <- have_name(vars) | !is_symbol

  if (any(needs_mutate)) {
    # TODO: use less of a hack
    if (inherits(.data, "data.frame")) {
      cols <- mutate_cols(.data, !!!vars)
      out <- dplyr_col_modify(.data, cols)
      col_names <- names(cols)
    } else {
      out <- mutate(.data, !!!vars)
      col_names <- names(exprs_auto_name(vars))
    }
  } else {
    out <- .data
    col_names <- names(exprs_auto_name(vars))
  }

  list(data = out, added_names = col_names)
}

quo_is_variable_reference <- function(quo) {
  if (quo_is_symbol(quo)) {
    return(TRUE)
  }

  if (quo_is_call(quo, n = 2)) {
    expr <- quo_get_expr(quo)

    if (node_cadr(expr) == sym(".data")) {
      fun <- node_car(expr)
      param <- node_cadr(node_cdr(expr))

      if (fun == sym("$") && (is_symbol(param) || is_string(param))) {
        return(TRUE)
      }

      if (fun == sym("[[") && is_string(param)) {
        return(TRUE)
      }
    }
  }

  FALSE
}

#' Default value for .drop argument of group_by
#'
#' @param .tbl A data frame
#'
#' @return `TRUE` unless `.tbl` is a grouped data frame that was previously
#'   obtained by `group_by(.drop = FALSE)`
#'
#' @examples
#' group_by_drop_default(iris)
#'
#' iris %>%
#'   group_by(Species) %>%
#'   group_by_drop_default()
#'
#' iris %>%
#'   group_by(Species, .drop = FALSE) %>%
#'   group_by_drop_default()
#'
#' @keywords internal
#' @export
group_by_drop_default <- function(.tbl) {
  UseMethod("group_by_drop_default")
}

#' @export
group_by_drop_default.default <- function(.tbl) {
  TRUE
}

#' @export
group_by_drop_default.grouped_df <- function(.tbl) {
  tryCatch({
    !identical(attr(group_data(.tbl), ".drop"), FALSE)
  }, error = function(e){
    TRUE
  })
}
