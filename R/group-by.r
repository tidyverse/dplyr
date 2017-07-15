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
#' @param .data a tbl
#' @param ... Variables to group by. All tbls accept variable names.
#'   Some tbls will accept functions of variables. Duplicated groups
#'   will be silently dropped.
#' @param add When `add = FALSE`, the default, `group_by()` will
#'   override existing groups. To add to the existing groups, use 
#'   `add = TRUE`.
#' @inheritParams filter
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
group_by <- function(.data, ..., add = FALSE) {
  UseMethod("group_by")
}
#' @export
group_by.default <- function(.data, ..., add = FALSE) {
  group_by_(.data, .dots = compat_as_lazy_dots(...), add = add)
}
#' @export
#' @rdname se-deprecated
#' @inheritParams group_by
group_by_ <- function(.data, ..., .dots = list(), add = FALSE) {
  UseMethod("group_by_")
}

#' @rdname group_by
#' @export
#' @param x A [tbl()]
ungroup <- function(x, ...) {
  UseMethod("ungroup")
}

#' Prepare for grouping.
#'
#' Performs standard operations that should happen before individual methods
#' process the data. This includes mutating the tbl to add new grouping columns
#' and updating the groups (based on add)
#'
#' @return A list
#'   \item{data}{Modified tbl}
#'   \item{groups}{Modified groups}
#' @export
#' @keywords internal
group_by_prepare <- function(.data, ..., .dots = list(), add = FALSE) {
  new_groups <- c(quos(...), compat_lazy_dots(.dots, caller_env()))

  # If any calls, use mutate to add new columns, then group by those
  is_symbol <- map_lgl(new_groups, quo_is_symbol)
  named <- have_name(new_groups)

  needs_mutate <- named | !is_symbol
  if (any(needs_mutate)) {
    .data <- mutate(.data, !!! new_groups[needs_mutate])
  }

  # Once we've done the mutate, we no longer need lazy objects, and
  # can instead just use symbols
  new_groups <- exprs_auto_name(new_groups, printer = tidy_text)
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

#' Return grouping variables
#'
#' `group_vars()` returns a character vector; `groups()` returns a list of
#' symbols.
#'
#' @param x A [tbl()]
#' @export
#' @examples
#' df <- tibble(x = 1, y = 2) %>% group_by(x, y)
#' group_vars(df)
#' groups(df)
groups <- function(x) {
  UseMethod("groups")
}

#' @rdname groups
#' @export
group_vars <- function(x) {
  UseMethod("group_vars")
}

#' @export
group_vars.default <- function(x) {
  deparse_names(groups(x))
}

