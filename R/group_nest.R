
group_nest_impl <- function(.data, key_var){
  to_nest <- select(ungroup(.data), setdiff(tbl_vars(.data), group_vars(.data)))

  out <- .data %>%
    group_data() %>%
    mutate(!!key_var := map(.data$.rows, function(.x) to_nest[.x,, drop = FALSE])) %>%
    select(-".rows")

  attr(out[[key_var]], "ptype") <- to_nest[integer(), , drop = FALSE]
  out
}

#' Nest a tibble using a grouping specification
#'
#' \badgeexperimental
#'
#' @family grouping functions
#'
#' Conceptually, `group_nest` is the same as `group_by()` + `tidyr::nest()`.
#'
#' @param .data a tbl
#' @param ... Variables to nest by, ignored if `.data` is already a grouped_df.
#' @param .key the name of the list column
#'
#' @details
#' When `.data` is an ungrouped data frame, the `...` specifiy the columns to group *by* are are forwarded
#' to either [group_by()], [group_by_at()] or [group_by_if()].
#'
#' When `.data` is a grouped data frame already, its grouping is used. A warning is issued if `...`
#' is used with a grouped data frame.
#'
#' @return A tbl with one row per unique combination of the grouping variables. The first columns are the grouping variables,
#' followed by a list column of tibbles with matching rows of the remaining columns.
#'
#' @keywords internal
#' @examples
#'
#' # using group_nest() or its column wise wariants on a ungrouped data frame
#' starwars %>%
#'   group_nest(species, homeworld)
#'
#' starwars %>%
#'   group_nest_at(vars(ends_with("_color")))
#'
#' starwars %>%
#'   group_by_if(is.numeric)
#'
#' # using group_nest() on a grouped data frame
#' starwars %>%
#'   group_by(species, homeworld) %>%
#'   group_nest()
#'
#' # can be useful when the grouped data has been altered before the nesting
#' # in this example the output is nested by homeworld after the summarise
#' # has peeled off one level of grouping
#' starwars %>%
#'   group_by(homeworld, species) %>%
#'   summarise(n = n()) %>%
#'   group_nest()
#'
#' @export
group_nest <- function(.data, ..., .key = "data"){
  UseMethod("group_nest")
}

#' @export
group_nest.default <- function(.data, ..., .key = "data") {
  abort("unsupported")
}

#' @export
group_nest.data.frame <- function(.data, ..., .key = "data") {
  group_nest_impl(
    group_by(.data, ...),
    key_var = quo_name(enexpr(.key))
  )
}

#' @export
group_nest.grouped_df <- function(.data, ..., .key = "data") {
  if (dots_n(...)) {
    warn("... is ignored in group_nest(<grouped_df>), please use group_by(..., add = TRUE) %>% group_nest()")
  }
  group_nest_impl(.data, key_var = quo_name(enexpr(.key)))
}

#' @rdname group_nest
#' @export
group_nest_at <- function(.data, ..., .key = "data") {
  group_nest(group_by_at(.data, ...), .key = quo_name(enexpr(.key)))
}

#' @rdname group_nest
#' @export
group_nest_if <- function(.data, ..., .key = "data") {
  group_nest(group_by_if(.data, ...), .key = quo_name(enexpr(.key)))
}
