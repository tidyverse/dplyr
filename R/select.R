#' Subset columns using their names and types
#'
#' @description
#'
#' Select (and optionally rename) variables in a data frame, using a concise
#' mini-language that makes it easy to refer to variables based on their name
#' (e.g. `a:f` selects all columns from `a` on the left to `f` on the
#' right). You can also use predicate functions like [is.numeric] to select
#' variables based on their properties.
#'
#'
#' ## Overview of selection features
#'
#' ```{r, child = "man/rmd/overview.Rmd"}
#' ```
#'
#' @inheritParams arrange
#' @param ... <[`tidy-select`][dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables.
#' @return
#' An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * Rows are not affected.
#' * Output columns are a subset of input columns, potentially with a different
#'   order. Columns will be renamed if `new_name = old_name` form is used.
#' * Data frame attributes are preserved.
#' * Groups are maintained; you can't select off grouping variables.
#'
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("select")}.
#'
#' @section Examples:
#'
#' ```{r, echo = FALSE, results = "asis"}
#' result <- rlang::with_options(
#'   knitr::knit_child("man/rmd/select.Rmd"),
#'   tibble.print_min = 4,
#'   tibble.max_extra_cols = 8,
#'   digits = 2,
#'   crayon.enabled = FALSE,
#'   cli.unicode = FALSE,
#'   width = 80
#' )
#' cat(result, sep = "\n")
#' ```
#'
#' @family single table verbs
#' @export
select <- function(.data, ...) {
  UseMethod("select")
}
#' @export
select.list <- function(.data, ...) {
  abort("`select()` doesn't handle lists.")
}

#' @export
select.data.frame <- function(.data, ...) {
  error_call <- dplyr_error_call()

  loc <- tidyselect::eval_select(
    expr(c(...)),
    data = .data,
    error_call = error_call
  )
  loc <- ensure_group_vars(loc, .data, notify = TRUE)

  dplyr_col_select(.data, loc, names(loc))
}


# Helpers -----------------------------------------------------------------

ensure_group_vars <- function(loc, data, notify = TRUE) {
  group_loc <- match(group_vars(data), names(data))
  missing <- setdiff(group_loc, loc)

  if (length(missing) > 0) {
    vars <- names(data)[missing]
    added_group_loc <- set_names(missing, vars)

    # don't add grouping variables with same name as new variable (#5841)
    added_group_loc <- added_group_loc[! vars %in% names(loc)]

    if (length(added_group_loc) > 0 && notify) {
      inform(glue(
        "Adding missing grouping variables: ",
        paste0("`", names(added_group_loc), "`", collapse = ", ")
      ))
    }

    loc <- c(added_group_loc, loc)
  }

  loc
}
