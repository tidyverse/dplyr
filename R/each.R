#' Select variables from the current group
#'
#' Select a tibble made of a tidy selection of columns
#' and the rows of the current group
#'
#' @param ... tidy selection of columns
#'
#' @examples
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(sepal = head(pick(starts_with("Sepal")), 2L))
#'
#' @export
pick <- function(...) {
  mask <- peek_mask()
  vars <- vars_select(peek_vars(), ...)
  mask$pick(vars)
}

#' @importFrom tidyselect peek_vars vars_select
#' @export
by_column <- function(df, funs = identity) {
  colwise(funs)(df)
}

#' Apply a set of functions to a set of columns
#'
#' Creates a data frame by applying a set of functions to a tidy
#' selection of columns in the current slice
#'
#' @param select tidy selection of columns, forwarded to [pick()]
#' @param funs Functions to apply to each of the selected columns. Possible
#'   values are:
#'
#'   - A single function
#'   - A single quosure style lambda, e.g. `~ mean(.x, na.rm = TRUE)`
#'   - A named list of functions and/or lambdas
#'
#'  @param .name A [glue::glue()] pattern used to name the result columns.
#'    When unspecified, the default naming depend on the other arguments
#'
#'  @param .unpack Whether to un pack the results in a flat data frame
#'    or keep the results of each function packed together.
#'
#'  @return A tibble
#'
#' @examples
#' # A single function, results are unpacked and named after the selected columns
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean))
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), ~mean(.x, na.rm = TRUE)))
#'
#'
#' # If we leave things packed, both columns are packed in the "fn" column
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean, .unpack = FALSE))
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), ~mean(.x, na.rm = TRUE)), .unpack = FALSE))
#'
#' # we can have control on the name of pack either by using a named list of
#' # functions or by giving a name to the expression in summarise()
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(packname = across(starts_with("Sepal"), mean))
#'
#' # With a named list, if there are multiple columns selected
#' # and the results are unpacked, the default name includes both the
#' # function name and the column name, separated by an underscore
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(
#'     across(starts_with("Sepal"), list(mean = mean, sd = sd))
#'   )
#' # this is also the case when the list has only one function
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(
#'     across(starts_with("Sepal"), list(mean = mean))
#'   )
#'
#' # If there is only one selected column, the results are named
#' # after the function names
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(
#'     across("Sepal.Length", list(mean = mean, sd = sd))
#'   )
#'
#' # When results are left packed, the packs are named after the functions
#' # and the results within the packs are named after the columns
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(
#'     across(starts_with("Sepal"), list(mean = mean, sd = sd), .unpack = FALSE)
#'   )
#'
#' # The .name parameter can be used to overwrite the defaults as seen above
#' # with a glue pattern that may use {fn} and {var} to stand for the current
#' # function name and column
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(starts_with("Sepal"), mean, .name = "{toupper(var)}"))
#'
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(packname = across(starts_with("Sepal"), mean, .name = "{toupper(var)}"))
#'
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(
#'     across(starts_with("Sepal"), list(mean = mean, sd = sd),
#'             .name = "{toupper(fn)}_{sub('Sepal.', '', var)}"
#'     )
#'   )
#'
#' # overwritting the default so that the name of the column still appear
#' # even though there is only one column selected
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(
#'     across("Sepal.Length", list(mean = mean, sd = sd), .name = "{fn}_{var}")
#'   )
#'
#' # when results are left packed, the default is to not include the function in the
#' # names of columns that are packed
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(
#'     across(starts_with("Sepal"), list(mean = mean, sd = sd), .unpack = FALSE, .name = "{fn}_{var}")
#'   )
#'
#' @export
across <- function(select, funs = identity) {
  colwise(funs)(pick({{select}}))
}

#' @export
current_key <- function() {
  peek_mask()$current_key()
}

#' @export
colwise <- function(funs = identity) {
  single_function <- is.function(funs) || is_formula(funs)
  if (single_function) {
    funs <- as_function(funs)
  } else {
    if (is.null(names(funs))) {
      abort("funs should be a single function, a single formula, or a named list of functions or formulas")
    }
    funs <- map(funs, as_function)
  }

  function(df) {
    if (single_function) {
      as_tibble(imap(df, funs))
    } else {
      results <- map(funs, function(f) {
        as_tibble(imap(df, f))
      })
      tibble(!!!results)
    }
  }
}
