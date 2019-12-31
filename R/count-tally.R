#' Count/tally observations by group
#'
#' @description
#' `count()` lets you quickly count the unique values of a variable, or
#' unique combinations of multiple variables. `tally()` is a lower-level
#' helper that works on already grouped data. Both can perform weighted counts,
#' if you give `wt` the name of a variable to weight by.
#'
#' `count()` and `tally()` are shortcuts for `summarise()`; `add_count()`
#' and `add_tally()` perform the identical operations but use `mutate()`
#' instead of `summarise()` so that they add a new column with group-wise
#' counts.
#'
#' @param x A [tbl()] to tally/count.
#' @param ... Variables to group by.
#' @param wt (Optional) If omitted, will count the number of rows.
#'   If specified, will perform a "weighted" tally by summing the (non-missing)
#'   values of variable `wt`.
#'
#'   If omitted, and column `n` exists, it will automatically be used as a
#'   weighting variable, although you will have to specify `name` to provide
#'   a new name for the output.
#' @param sort If `TRUE` will sort output in descending order of `n`.
#' @param name The name of the new column in the output.
#'
#'   If omitted, it will default to `n`. If there's already a column called `n`,
#'   it will error, and require you to specify the name.
#' @param .drop see [group_by()]
#' @return A tbl, grouped the same way as the input.
#' @export
#' @examples
#' # count() is a convenient way to get a sense of the distribution of
#' # values in a dataset
#' starwars %>% count(species)
#' starwars %>% count(species, sort = TRUE)
#'
#' # use the `wt` argument to perform a weighted count. This is useful
#' # when the data has already been aggregated once
#' df <- tribble(
#'   ~name,    ~gender,   ~runs,
#'   "Max",    "male",       10,
#'   "Sandra", "female",      1,
#'   "Susan",  "female",      4
#' )
#' # counts rows:
#' df %>% count(gender)
#' # counts runs:
#' df %>% count(gender, wt = runs)
#'
#' # tally() is a lower-level function that assumes you've done the grouping
#' starwars %>% tally()
#' starwars %>% group_by(species) %>% tally()
#'
#' # both count() and tally() have add_ variants that work like
#' # mutate() instead of summarise
#' df %>% add_count(gender, wt = runs)
#' df %>% add_tally(wt = runs)
tally <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  n <- tally_n(x, {{ wt }})
  name <- check_name(x, name)
  out <- summarise(x, !!name := !!n)

  if (sort) {
    arrange(out, desc(!!sym(name)))
  } else {
    out
  }
}

#' @rdname tally
#' @export
add_tally <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  n <- tally_n(x, {{ wt }})
  name <- check_name(x, name)
  out <- mutate(x, !!name := !!n)

  if (sort) {
    arrange(out, desc(!!sym(name)))
  } else {
    out
  }
}

#' @export
#' @rdname tally
count <- function(x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = group_by_drop_default(x)) {
  groups <- group_vars(x)
  if (!missing(...)) {
    x <- .group_by_static_drop(x, ..., add = TRUE, .drop = .drop)
  }

  x <- tally(x, wt = !!enquo(wt), sort = sort, name = name)
  x <- .group_by_static_drop(x, !!!syms(groups), add = FALSE, .drop = .drop)
  x
}

#' @rdname tally
#' @export
add_count <- function(x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = group_by_drop_default(x)) {
  groups <- group_vars(x)
  if (!missing(...)) {
    x <- .group_by_static_drop(x, ..., add = TRUE, .drop = .drop)
  }

  x <- add_tally(x, wt = !!enquo(wt), sort = sort, name = name)
  x <- .group_by_static_drop(x, !!!syms(groups), add = FALSE, .drop = .drop)
  x
}

# Helpers -----------------------------------------------------------------

tally_n <- function(x, wt) {
  wt <- enquo(wt)
  if (quo_is_null(wt) && "n" %in% tbl_vars(x)) {
    inform("Using `n` as weighting variable")
    wt <- quo(n)
  }

  if (quo_is_null(wt)) {
    quo(n())
  } else {
    quo(sum(!!wt, na.rm = TRUE))
  }
}

check_name <- function(df, name) {
  if (is.null(name)) {
    if ("n" %in% tbl_vars(df)) {
      glubort(
        "Column 'n' is already present in output\n",
        "* Use `name = \"new_name\"` to pick a new name"
      )
    }
    return("n")
  }

  if (!is.character(name) || length(name) != 1) {
    abort("`name` must be a single string")
  }

  name
}
