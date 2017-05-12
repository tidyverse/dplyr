#' Count/tally observations by group
#'
#' @description
#' `tally()` is a convenient wrapper for summarise that will either call
#' [n()] or \code{\link{sum}(n)} depending on whether you're tallying
#' for the first time, or re-tallying. `count()` is similar but calls
#' [group_by()] before and [ungroup()] after.
#'
#' `add_tally()` adds a column "n" to a table based on the number
#' of items within each existing group, while `add_count()` is a shortcut that
#' does the grouping as well. These functions are to [tally()]
#' and [count()] as [mutate()] is to [summarise()]:
#' they add an additional column rather than collapsing each group.
#'
#' @note
#' The column name in the returned data is usually `n`, even if you
#' have supplied a weight.
#'
#' If the data already already has a column named `n`, the output column
#' will be called `nn`. If the table already has columns called `n` and `nn`
#' then the column returned will be `nnn`, and so on.
#'
#' There is currently no way to control the output variable name - if you
#' need to change the default, you'll have to write the [summarise()]
#' yourself.
#'
#' @param x a [tbl()] to tally/count.
#' @param ... Variables to group by.
#' @param wt (Optional) If omitted, will count the number of rows. If
#'   specified, will perform a "weighted" tally by summing the
#'   (non-missing) values of variable `wt`. This argument is
#'   automatically [quoted][rlang::quo] and later
#'   [evaluated][rlang::eval_tidy] in the context of the data
#'   frame. It supports [unquoting][rlang::quasiquotation]. See
#'   `vignette("programming")` for an introduction to these concepts.
#' @param sort if `TRUE` will sort output in descending order of `n`
#' @return A tbl, grouped the same way as `x`.
#' @export
#' @examples
#' # tally() is short-hand for mutate()
#' mtcars %>% tally()
#' # count() is a short-hand for group_by() + tally()
#' mtcars %>% count(cyl)
#'
#' # add_tally() is short-hand for mutate()
#' mtcars %>% add_tally()
#' # add_count() is a short-hand for group_by() + add_tally()
#' mtcars %>% add_count(cyl)
#'
#' # count and tally are designed so that you can call
#' # them repeatedly, each time rolling up a level of detail
#' species <- starwars %>% count(species, homeworld, sort = TRUE)
#' species
#' species %>% count(species, sort = TRUE)
#'
#' # add_count() is useful for groupwise filtering
#' # e.g.: show only species that have a single member
#' starwars %>%
#'   add_count(species) %>%
#'   filter(n == 1)
tally <- function(x, wt, sort = FALSE) {
  wt <- enquo(wt)

  if (quo_is_missing(wt) && "n" %in% names(x)) {
    inform("Using `n` as weighting variable")
    wt <- quo(n)
  }

  if (quo_is_missing(wt) || quo_is_null(wt)) {
    n <- quo(n())
  } else {
    n <- quo(sum(!! wt, na.rm = TRUE))
  }

  n_name <- n_name(tbl_vars(x))
  out <- summarise(x, !! n_name := !! n)

  if (sort) {
    arrange(out, desc(!! sym(n_name)))
  } else {
    out
  }
}
#' @rdname se-deprecated
#' @inheritParams tally
#' @export
tally_ <- function(x, wt, sort = FALSE) {
  wt <- compat_lazy(wt, caller_env())
  tally(x, wt = !! wt, sort = sort)
}

n_name <- function(x) {
  name <- "n"
  while (name %in% x) {
    name <- paste0(name, "n")
  }

  name

}

#' @export
#' @rdname tally
count <- function(x, ..., wt = NULL, sort = FALSE) {
  groups <- group_vars(x)

  x <- group_by(x, ..., add = TRUE)
  x <- tally(x, wt = !! enquo(wt), sort = sort)
  x <- group_by(x, !!! syms(groups), add = FALSE)
  x
}
#' @export
#' @rdname se-deprecated
count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  vars <- compat_lazy_dots(vars, caller_env())
  wt <- wt %||% quo(NULL)
  wt <- compat_lazy(wt, caller_env())
  count(x, !!! vars, wt = !! wt, sort = sort)
}

#' @rdname tally
#' @export
add_tally <- function(x, wt, sort = FALSE) {
  wt <- enquo(wt)

  if (quo_is_missing(wt) && "n" %in% names(x)) {
    inform("Using `n` as weighting variable")
    wt <- quo(n)
  }

  if (quo_is_missing(wt) || quo_is_null(wt)) {
    n <- quo(n())
  } else {
    n <- quo(sum(!! wt, na.rm = TRUE))
  }

  n_name <- n_name(tbl_vars(x))
  out <- mutate(x, !! n_name := !! n)

  if (sort) {
    out <- arrange(out, desc(!! sym(n_name)))
  }

  grouped_df(out, group_vars(x))
}
#' @rdname se-deprecated
#' @export
add_tally_ <- function(x, wt, sort = FALSE) {
  wt <- compat_lazy(wt, caller_env())
  add_tally(x, !! wt, sort = sort)
}


#' @rdname tally
#' @export
add_count <- function(x, ..., wt = NULL, sort = FALSE) {
  g <- group_vars(x)
  grouped <- group_by(x, ..., add = TRUE)

  out <- add_tally(grouped, wt = !! enquo(wt), sort = sort)
  grouped_df(out, g)
}
#' @rdname se-deprecated
#' @export
add_count_ <- function(x, vars, wt = NULL, sort = FALSE) {
  vars <- compat_lazy_dots(vars, caller_env())
  wt <- wt %||% quo(NULL)
  wt <- compat_lazy(wt, caller_env())
  add_count(x, !!! vars, wt = !! wt, sort = sort)
}
