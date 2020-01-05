#' Create or transform variables
#'
#' `mutate()` adds new variables and preserves existing ones;
#' `transmute()` adds new variables and drops existing ones.  Both
#' functions preserve the number of rows of the input.
#' New variables overwrite existing variables of the same name.
#'
#' @section Useful functions available in calculations of variables:
#'
#' * [`+`], [`-`], [log()], etc., for their usual mathematical meanings
#'
#' * [lead()], [lag()]
#'
#' * [dense_rank()], [min_rank()], [percent_rank()], [row_number()],
#'   [cume_dist()], [ntile()]
#'
#' * [cumsum()], [cummean()], [cummin()], [cummax()], [cumany()], [cumall()]
#'
#' * [na_if()], [coalesce()]
#'
#' * [if_else()], [recode()], [case_when()]
#'
#' @section Grouped tibbles:
#'
#' Because mutating expressions are computed within groups, they may
#' yield different results on grouped tibbles. This will be the case
#' as soon as an aggregating, lagging, or ranking function is
#' involved. Compare this ungrouped mutate:
#'
#' ```
#' starwars %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#' ```
#'
#' With the grouped equivalent:
#'
#' ```
#' starwars %>%
#'   group_by(gender) %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#' ```
#'
#' The former normalises `mass` by the global average whereas the
#' latter normalises by the averages within gender levels.
#'
#' Note that you can't overwrite a grouping variable within
#' `mutate()`.
#'
#' `mutate()` does not evaluate the expressions when the group is empty.
#'
#' @section Scoped mutation and transmutation:
#'
#' The three [scoped] variants of `mutate()` ([mutate_all()],
#' [mutate_if()] and [mutate_at()]) and the three variants of
#' `transmute()` ([transmute_all()], [transmute_if()],
#' [transmute_at()]) make it easy to apply a transformation to a
#' selection of variables.
#'
#' @export
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Name-value pairs of expressions,
#'   each with length 1 or the same length as the number of rows in the group
#'   (if using [group_by()]) or in the entire input (if not using groups).
#'   The name of each argument will be the name of a new variable, and the
#'   value will be its corresponding value. Use a `NULL` value in `mutate`
#'   to drop a variable.  New variables overwrite existing variables
#'   of the same name.
#' @family single table verbs
#' @return An object of the same class as `.data`.
#' @examples
#' # Newly created variables are available immediately
#' mtcars %>% as_tibble() %>% mutate(
#'   cyl2 = cyl * 2,
#'   cyl4 = cyl2 * 2
#' )
#'
#' # You can also use mutate() to remove variables and
#' # modify existing variables
#' mtcars %>% as_tibble() %>% mutate(
#'   mpg = NULL,
#'   disp = disp * 0.0163871 # convert to litres
#' )
#'
#'
#' # window functions are useful for grouped mutates
#' mtcars %>%
#'  group_by(cyl) %>%
#'  mutate(rank = min_rank(desc(mpg)))
#' # see `vignette("window-functions")` for more details
#'
#' # You can drop variables by setting them to NULL
#' mtcars %>% mutate(cyl = NULL)
#'
#' # mutate() vs transmute --------------------------
#' # mutate() keeps all existing variables
#' mtcars %>%
#'   mutate(displ_l = disp / 61.0237)
#'
#' # transmute keeps only the variables you create
#' mtcars %>%
#'   transmute(displ_l = disp / 61.0237)
#'
#'
#' # The mutate operation may yield different results on grouped
#' # tibbles because the expressions are computed within groups.
#' # The following normalises `mass` by the global average:
#' starwars %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#'
#' # Whereas this normalises `mass` by the averages within gender
#' # levels:
#' starwars %>%
#'   group_by(gender) %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#'
#' # Note that you can't overwrite grouping variables:
#' gdf <- mtcars %>% group_by(cyl)
#' try(mutate(gdf, cyl = cyl * 100))
#'
#'
#' # Refer to column names stored as strings with the `.data` pronoun:
#' vars <- c("mass", "height")
#' mutate(starwars, prod = .data[[vars[[1]]]] * .data[[vars[[2]]]])
#' # Learn more in ?dplyr_tidy_eval
mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' @rdname mutate
#' @export
transmute <- function(.data, ...) {
  UseMethod("transmute")
}

