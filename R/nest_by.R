
nest_by_impl <- function(.data, key_var){
  to_nest <- select(ungroup(.data), setdiff(tbl_vars(.data), group_vars(.data)))

  .data %>%
    group_data() %>%
    mutate(!!key_var := map(.data$.rows, function(.x) to_nest[.x,, drop = FALSE])) %>%
    select(-".rows")
}

#' Nest by one or more variables
#'
#' \badgeexperimental
#'
#' Conceptually, `nest_by` is the same as `group_by()` + `tidyr::nest()`.
#'
#' @param .data a tbl
#' @param ... Variables to nest by. `nest_by` passes them to [group_by()], `nest_by_at` passes them to [group_by_at()] and `nest_by_if()` passes them to [group_by_if()]
#' @param .key the name of the list column
#'
#' @return A tbl with one row per unique combination of the grouping variables. The first columns are the grouping variables,
#' followed by a list column of tibbles with matching rows of the remaining columns.
#'
#' @keywords internal
#' @examples
#' starwars %>%
#'   nest_by(species, homeworld)
#'
#' starwars %>%
#'   nest_by_at(vars(ends_with("_color")))
#'
#' starwars %>%
#'   nest_by_if(is.numeric)
#' @export
nest_by <- function(.data, ..., .key = "data"){
  nest_by_impl(
    group_by(.data, ..., add = FALSE),
    key_var = quo_name(enexpr(.key))
  )
}

#' @rdname nest_by
#' @export
nest_by_at <- function(.data, ..., .key = "data"){
  nest_by_impl(
    group_by_at(.data, ..., .add = FALSE),
    key_var = quo_name(enexpr(.key))
  )
}

#' @rdname nest_by
#' @export
nest_by_if <- function(.data, ..., .key = "data"){
  nest_by_impl(
    group_by_if(.data, ..., .add = FALSE),
    key_var = quo_name(enexpr(.key))
  )
}
