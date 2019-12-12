#' Group by one or more variables
#'
#' @description
#' Most data operations are done on groups defined by variables.
#' `group_by()` takes an existing tbl and converts it into a grouped tbl
#' where operations are performed "by group". `ungroup()` removes grouping.
#'
#' @section Tbl types:
#'
#' `group_by()` is an S3 generic with methods for the three built-in
#' tbls. See the help for the corresponding classes and their manip
#' methods for more details:
#'
#' \itemize{
#'   \item data.frame: [grouped_df]
#'   \item data.table: [dtplyr::grouped_dt]
#'   \item SQLite: [src_sqlite()]
#'   \item PostgreSQL: [src_postgres()]
#'   \item MySQL: [src_mysql()]
#' }
#'
#' @section Scoped grouping:
#'
#' The three [scoped] variants ([group_by_all()], [group_by_if()] and
#' [group_by_at()]) make it easy to group a dataset by a selection of
#' variables.
#'
#' @family grouping functions
#' @param .data a tbl
#' @param ... Variables to group by. All tbls accept variable names.
#'   Some tbls will accept functions of variables. Duplicated groups
#'   will be silently dropped.
#' @param add When `add = FALSE`, the default, `group_by()` will
#'   override existing groups. To add to the existing groups, use
#'   `add = TRUE`.
#' @param .drop When `.drop = TRUE`, empty groups are dropped. See [group_by_drop_default()] for
#'   what the default value is for this argument.
#' @return A [grouped data frame][grouped_df()], unless the combination of `...` and `add`
#'   yields a non empty set of grouping columns, a regular (ungrouped) data frame
#'   otherwise.
#'
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
#' # a mutate/rename followed by a simple group_by
#' mtcars %>% group_by(vsam = vs + am)
#'
#' # By default, group_by overrides existing grouping
#' by_cyl %>%
#'   group_by(vs, am) %>%
#'   group_vars()
#'
#' # Use add = TRUE to instead append
#' by_cyl %>%
#'   group_by(vs, am, add = TRUE) %>%
#'   group_vars()
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
group_by <- function(.data, ..., add = FALSE, .drop = group_by_drop_default(.data)) {
  UseMethod("group_by")
}

#' @rdname group_by
#' @export
#' @param x A [tbl()]
ungroup <- function(x, ...) {
  UseMethod("ungroup")
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
group_by_prepare <- function(.data, ..., .dots = "DEFUNCT", add = FALSE) {
  new_groups <- enquos(...)
  if (!missing(.dots)) {
    # Used by dbplyr 1.4.2 so can't aggressively deprecate
    new_groups <- c(new_groups, compat_lazy_dots(.dots))
  }
  new_groups <- new_groups[!map_lgl(new_groups, quo_is_missing)]

  # If any calls, use mutate to add new columns, then group by those
  .data <- add_computed_columns(.data, new_groups)

  # Once we've done the mutate, we need to name all objects
  new_groups <- exprs_auto_name(new_groups)

  group_names <- names(new_groups)
  if (add) {
    group_names <- c(group_vars(.data), group_names)
  }
  group_names <- unique(group_names)

  list(
    data = .data,
    groups = syms(group_names),
    group_names = group_names
  )
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

      if (fun == sym("$") && (is_symbol(param) || (is_string(param) && length(param) == 1L))) {
        return(TRUE)
      }

      if (fun == sym("[[") && (is_string(param) && length(param) == 1L)) {
        return(TRUE)
      }
    }
  }

  FALSE
}

add_computed_columns <- function(.data, vars) {
  is_symbol <- map_lgl(vars, quo_is_variable_reference)
  named <- have_name(vars)

  needs_mutate <- named | !is_symbol

  # Shortcut necessary, otherwise all columns are analyzed in mutate(),
  # this can change behavior
  mutate_vars <- vars[needs_mutate]
  if (length(mutate_vars) == 0L) return(.data)

  mutate(.data, !!!mutate_vars)
}

#' Return grouping variables
#'
#' `group_vars()` returns a character vector; `groups()` returns a list of
#' symbols.
#'
#' @family grouping functions
#' @param x A [tbl()]
#'
#' @seealso [group_cols()] for matching grouping variables in
#'   [selection contexts][select].
#' @export
#' @examples
#' df <- tibble(x = 1, y = 2) %>% group_by(x, y)
#' group_vars(df)
#' groups(df)
groups <- function(x) {
  UseMethod("groups")
}

#' @export
groups.default <- function(x) NULL

#' @rdname groups
#' @export
group_vars <- function(x) {
  UseMethod("group_vars")
}

#' @export
group_vars.default <- function(x) {
  deparse_names(groups(x))
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
