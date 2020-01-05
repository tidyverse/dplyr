#' Arrange rows by variables
#'
#' Order tbl rows by an expression involving its variables.
#'
#' @section Locales:
#' The sort order for character vectors will depend on the collating sequence
#' of the locale in use: see [locales()].
#'
#' @section Missing values:
#' Unlike base sorting with `sort()`, `NA` are:
#' * always sorted to the end for local data, even when wrapped with `desc()`.
#' * treated differently for remote data, depending on the backend.
#'
#' @export
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Variables, or functions or
#'   variables. Use [desc()] to sort a variable in descending order.
#' @family single table verbs
#' @return An object of the same class as `.data`.
#' @examples
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, desc(disp))
#'
#' # grouped arrange ignores groups
#' by_cyl <- mtcars %>% group_by(cyl)
#' by_cyl %>% arrange(desc(wt))
#' # Unless you specifically ask:
#' by_cyl %>% arrange(desc(wt), .by_group = TRUE)
arrange <- function(.data, ...) {
  UseMethod("arrange")
}
