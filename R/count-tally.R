#' Count/tally observations by group
#'
#' @description
#' `tally()` is a convenient wrapper for summarise that will either call
#' [n()] or \code{\link{sum}(n)} depending on whether you're tallying
#' for the first time, or re-tallying. `count()` is similar but calls
#' [group_by()] before and [ungroup()] after. If the data is already
#' grouped, `count()` adds an additional group that is removed afterwards.
#'
#' `add_tally()` adds a column `n` to a table based on the number
#' of items within each existing group, while `add_count()` is a shortcut that
#' does the grouping as well. These functions are to [tally()]
#' and [count()] as [mutate()] is to [summarise()]:
#' they add an additional column rather than collapsing each group.
#'
#' @note
#' The column name in the returned data is given by the `name` argument,
#' set to `"n"` by default.
#'
#' If the data already has a column by that name, the output column
#' will be prefixed by an extra `"n"` as many times as necessary.
#'
#' @param x a [tbl()] to tally/count.
#' @param ... Variables to group by.
#' @param wt (Optional) If omitted (and no variable named `n` exists in the
#'   data), will count the number of rows.
#'   If specified, will perform a "weighted" tally by summing the
#'   (non-missing) values of variable `wt`. A column named `n` (but not `nn` or
#'   `nnn`) will be used as weighting variable by default in `tally()`, but not
#'   in `count()`. This argument is automatically [quoted][rlang::quo] and later
#'   [evaluated][rlang::eval_tidy] in the context of the data
#'   frame. It supports [unquoting][rlang::quasiquotation]. See
#'   `vignette("programming")` for an introduction to these concepts.
#' @param sort if `TRUE` will sort output in descending order of `n`
#' @param name The output column name. If omitted, it will be `n`.
#' @param .drop see [group_by()]
#' @return A tbl, grouped the same way as `x`.
#' @export
#' @examples
#' # tally() is short-hand for summarise()
#' mtcars %>% tally()
#' mtcars %>% group_by(cyl) %>% tally()
#' # count() is a short-hand for group_by() + tally()
#' mtcars %>% count(cyl)
#' # Note that if the data is already grouped, count() adds
#' # an additional group that is removed afterwards
#' mtcars %>% group_by(gear) %>% count(carb)
#'
#' # add_tally() is short-hand for mutate()
#' mtcars %>% add_tally()
#' # add_count() is a short-hand for group_by() + add_tally()
#' mtcars %>% add_count(cyl)
#'
#' # count() and tally() are designed so that you can call
#' # them repeatedly, each time rolling up a level of detail
#' species <-
#'  starwars %>%
#'  count(species, homeworld, sort = TRUE)
#' species
#' species %>% count(species, sort = TRUE)
#'
#' # Change the name of the newly created column:
#' species <-
#'  starwars %>%
#'  count(species, homeworld, sort = TRUE, name = "n_species_by_homeworld")
#' species
#' species %>%
#'  count(species, sort = TRUE, name = "n_species")
#'
#' # add_count() is useful for groupwise filtering
#' # e.g.: show details for species that have a single member
#' starwars %>%
#'   add_count(species) %>%
#'   filter(n == 1)
tally <- function(x, wt = NULL, sort = FALSE, name = "n") {
  wt <- enquo(wt)

  if (quo_is_missing(wt) && "n" %in% tbl_vars(x)) {
    inform("Using `n` as weighting variable")
    wt <- quo(n)
  }

  if (quo_is_missing(wt) || quo_is_null(wt)) {
    n <- quo(n())
  } else {
    n <- quo(sum(!!wt, na.rm = TRUE))
  }

  n_name <- n_name(group_vars(x), name)

  if (name != "n" && name %in% group_vars(x)) {
    abort(glue("Column `{name}` already exists in grouped variables"))
  }

  out <- summarise(x, !!n_name := !!n)

  if (sort) {
    arrange(out, desc(!!sym(n_name)))
  } else {
    out
  }
}
#' @rdname se-deprecated
#' @inheritParams tally
#' @export
tally_ <- function(x, wt, sort = FALSE) {
  signal_soft_deprecated(paste_line(
    "tally_() is deprecated. ",
    "Please use tally() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with tally() : https://tidyeval.tidyverse.org"
  ))

  wt <- compat_lazy(wt, caller_env())
  tally(x, wt = !!wt, sort = sort)
}

n_name <- function(x, name = "n") {
  while (name %in% x) {
    name <- paste0("n", name)
  }

  name
}

#' @export
#' @rdname tally
count <- function(x, ..., wt = NULL, sort = FALSE, name = "n", .drop = group_by_drop_default(x)) {
  groups <- group_vars(x)

  if (dots_n(...)) {
    x <- .group_by_static_drop(x, ..., add = TRUE, .drop = .drop)
  }
  x <- tally(x, wt = !!enquo(wt), sort = sort, name = name)
  x <- .group_by_static_drop(x, !!!syms(groups), add = FALSE, .drop = .drop)
  x
}
#' @export
#' @rdname se-deprecated
count_ <- function(x, vars, wt = NULL, sort = FALSE, .drop = group_by_drop_default(x)) {
  signal_soft_deprecated(paste_line(
    "count_() is deprecated. ",
    "Please use count() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with count() : https://tidyeval.tidyverse.org"
  ))

  vars <- compat_lazy_dots(vars, caller_env())
  wt <- wt %||% quo(NULL)
  wt <- compat_lazy(wt, caller_env())
  count(x, !!!vars, wt = !!wt, sort = sort, .drop = .drop)
}

#' @rdname tally
#' @export
add_tally <- function(x, wt, sort = FALSE, name = "n") {
  wt <- enquo(wt)

  if (quo_is_missing(wt) && "n" %in% tbl_vars(x)) {
    inform("Using `n` as weighting variable")
    wt <- quo(n)
  }

  if (quo_is_missing(wt) || quo_is_null(wt)) {
    n <- quo(n())
  } else {
    n <- quo(sum(!!wt, na.rm = TRUE))
  }

  n_name <- n_name(group_vars(x), name)

  if (name != "n" && name %in% group_vars(x)) {
    abort(glue("Column `{name}` already exists in grouped variables"))
  }

  out <- mutate(x, !!n_name := !!n)

  if (sort) {
    out <- arrange(out, desc(!!sym(n_name)))
  }

  out
}
#' @rdname se-deprecated
#' @export
add_tally_ <- function(x, wt, sort = FALSE) {
  signal_soft_deprecated(paste_line(
    "add_tally_() is deprecated. ",
    "Please use add_tally() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with add_tally() : https://tidyeval.tidyverse.org"
  ))

  wt <- compat_lazy(wt, caller_env())
  add_tally(x, !!wt, sort = sort)
}
#' @rdname tally
#' @export
add_count <- function(x, ..., wt = NULL, sort = FALSE, name = "n") {
  grouped <- group_by(x, ..., add = TRUE)
  out <- add_tally(grouped, wt = !!enquo(wt), sort = sort, name = name)
  group_by(out, !!!groups(x), .drop = group_by_drop_default(grouped))
}
#' @rdname se-deprecated
#' @export
add_count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  signal_soft_deprecated(paste_line(
    "add_count_() is deprecated. ",
    "Please use add_count() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with add_count() : https://tidyeval.tidyverse.org"
  ))

  vars <- compat_lazy_dots(vars, caller_env())
  wt <- wt %||% quo(NULL)
  wt <- compat_lazy(wt, caller_env())
  add_count(x, !!!vars, wt = !!wt, sort = sort)
}
