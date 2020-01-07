#' Arrange rows by variables
#'
#' Order data frame rows by the values of selected variables.
#' Unlike other dplyr verbs, `arrange()` largely ignores grouping; you
#' need to explicit mention grouping variables (or use  `by_group = TRUE`)
#' in order to group by them, and functions of variables are evaluated
#' once per data frame, not once per group.
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

#' @param .by_group If `TRUE`, will sort first by grouping variable. Applies to
#'   grouped data frames only.
#' @rdname arrange
#' @export
arrange.tbl_df <- function(.data, ..., .by_group = FALSE) {
  arrange_data_frame(.data, ..., .by_group = .by_group)
}


#' @export
arrange.data.frame <- function(.data, ..., .by_group = FALSE) {
  as.data.frame(arrange(as_tibble(.data), ..., .by_group = .by_group))
}


# Helpers -----------------------------------------------------------------

arrange_data_frame <- function(.data, ..., .by_group = FALSE) {
  if (dots_n(...) == 0L) {
    return(.data)
  }

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
  data <- transmute(as_tibble(.data), !!!quosures)

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

  orders <- exec("order", !!!proxies, decreasing = FALSE, na.last = TRUE)

  out <- vec_slice(as_tibble(.data), orders)
  if (is_grouped_df(.data)) {
    out <- grouped_df(out, vars = group_vars(.data), drop = group_by_drop_default(.data))
  }
  out
}

