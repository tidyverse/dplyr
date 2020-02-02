#' Arrange rows by column values
#'
#' @description
#' `arrange()` order the rows of a data frame rows by the values of selected
#' columns.
#'
#' Unlike other dplyr verbs, `arrange()` largely ignores grouping; you
#' need to explicit mention grouping variables (or use  `by_group = TRUE`)
#' in order to group by them, and functions of variables are evaluated
#' once per data frame, not once per group.
#'
#' @details
#' ## Locales
#' The sort order for character vectors will depend on the collating sequence
#' of the locale in use: see [locales()].
#'
#' ## Missing values
#' Unlike base sorting with `sort()`, `NA` are:
#' * always sorted to the end for local data, even when wrapped with `desc()`.
#' * treated differently for remote data, depending on the backend.
#'
#' @return
#' An object of the same type as `.data`.
#'
#' * All rows appear in the output, but (usually) in a different place.
#' * Columns are not modified.
#' * Groups are not modified.
#' * Data frame attributes are preserved.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("arrange")}.
#' @export
#' @param .data A data frame, data frame extension (e.g. a tibble), or a
#'   lazy data frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for
#'   more details.
#' @param ... <[`data-masking`][dplyr_data_masking]> Variables, or functions or
#'   variables. Use [desc()] to sort a variable in descending order.
#' @family single table verbs
#' @examples
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, desc(disp))
#'
#' # grouped arrange ignores groups
#' by_cyl <- mtcars %>% group_by(cyl)
#' by_cyl %>% arrange(desc(wt))
#' # Unless you specifically ask:
#' by_cyl %>% arrange(desc(wt), .by_group = TRUE)
#' #' # using tidy eval embracing
#' tidy_eval_arrange <- function(.data, arrange_var) {
#'   .data %>%
#'     arrange({{ arrange_var }})
#' }
#' tidy_eval_arrange(mtcars, mpg)
#' # using across()
#' iris %>% arrange(across(starts_with("Sepal"), desc))
arrange <- function(.data, ..., .by_group = FALSE) {
  UseMethod("arrange")
}

#' @param .by_group If `TRUE`, will sort first by grouping variable. Applies to
#'   grouped data frames only.
#' @rdname arrange
#' @export
arrange.data.frame <- function(.data, ..., .by_group = FALSE) {
  if (missing(...)) {
    return(.data)
  }

  loc <- arrange_rows(.data, ..., .by_group = .by_group)
  dplyr_row_slice(.data, loc)
}

# Helpers -----------------------------------------------------------------

arrange_rows <- function(.data, ..., .by_group = FALSE) {

  if (.by_group) {
    dots <- c(quos(!!!groups(.data)), enquos(...))
  } else {
    dots <- enquos(...)
  }

  directions <- map_chr(dots, function(quosure) {
    if(quo_is_call(quosure, "desc")) "desc" else "asc"
  })

  quosures <- map(dots, function(quosure) {
    if (quo_is_call(quosure, "desc")) {
      quosure <- new_quosure(
        node_cadr(quo_get_expr(quosure)),
        quo_get_env(quosure)
      )
    }
    quosure
  })
  # give the quosures arbitrary names so that
  # data has the right number of columns below after transmute()
  names(quosures) <- paste0("^^--arrange_quosure_", seq_along(quosures))

  # TODO: not quite that because when the quosure is some expression
  #       it should be evaluated by groups.
  #       for now this abuses transmute so that we get just one
  #       column per quosure
  #
  #       revisit when we have something like mutate_one() to
  #       evaluate one quosure in the data mask
  data <- transmute(ungroup(.data), !!!quosures)

  # we can't just use vec_compare_proxy(data) because we need to apply
  # direction for each column, so we get a list of proxies instead
  # and then mimic vctrs:::order_proxy
  #
  # should really be map2(quosures, directions, ...)
  proxies <- map2(unname(data), directions, function(column, direction) {
    proxy <- vec_proxy_compare(column, relax = TRUE)
    desc <- identical(direction, "desc")
    if (is.data.frame(proxy)) {
      proxy <- order(vec_order(proxy,
        direction = direction,
        na_value = if(desc) "smallest" else "largest"
      ))
    } else if(desc) {
      proxy <- desc(proxy)
    }
    proxy
  })

  exec("order", !!!proxies, decreasing = FALSE, na.last = TRUE)
}
