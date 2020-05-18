#' Count observations by group
#'
#' @description
#' `count()` lets you quickly count the unique values of one or more variables:
#' `df %>% count(a, b)` is roughly equivalent to
#' `df %>% group_by(a, b) %>% summarise(n = n())`.
#' `count()` is paired with `tally()`, a lower-level helper that is equivalent
#' to `df %>% summarise(n = n())`. Supply `wt` to perform weighted counts,
#' switching the summary from from `n = n()` to `n = sum(wt)`.
#'
#' `add_count()` are `add_tally()` are equivalents to `count()` and `tally()`
#' the use `mutate()` instead of `summarise()` so that they add a new column
#' with group-wise counts.
#'
#' @param x A data frame, data frame extension (e.g. a tibble), or a
#'   lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... <[`data-masking`][dplyr_data_masking]> Variables to group by.
#' @param wt <[`data-masking`][dplyr_data_masking]> If omitted, will count the
#'   number of rows. If specified, will perform a "weighted" count by summing
#'   the (non-missing) values of variable `wt`.
#'
#'   If omitted, and column `n` exists, it will automatically be used as a
#'   weighting variable, although you will have to specify `name` to provide
#'   a new name for the output.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param name The name of the new column in the output.
#'
#'   If omitted, it will default to `n`. If there's already a column called `n`,
#'   it will error, and require you to specify the name.
#' @param .drop For `count()`: if `FALSE` will include counts for empty groups
#'   (i.e. for levels of factors that don't exist in the data). Deprecated in
#'   `add_count()` since it didn't actually affect the output.
#' @return
#' An object of the same type as `.data`. `count()` and `add_count()`
#' group transiently, so the output has the same groups as the input.
#' @export
#' @examples
#' # count() is a convenient way to get a sense of the distribution of
#' # values in a dataset
#' starwars %>% count(species)
#' starwars %>% count(species, sort = TRUE)
#' starwars %>% count(sex, gender, sort = TRUE)
#' starwars %>% count(birth_decade = round(birth_year, -1))
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
count <- function(x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = group_by_drop_default(x)) {

  if (!missing(...)) {
    out <- group_by(x, ..., .add = TRUE, .drop = .drop)
  } else {
    out <- x
  }

  out <- tally(out, wt = !!enquo(wt), sort = sort, name = name)
  if (is.data.frame(x)) {
    out <- dplyr_reconstruct(out, x)
  }
  out
}

#' @export
#' @rdname count
tally <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  n <- tally_n(x, {{ wt }})
  name <- check_name(x, name)

  out <- local({
    old.options <- options(dplyr.summarise.inform = FALSE)
    on.exit(options(old.options))
    summarise(x, !!name := !!n)
  })

  if (sort) {
    arrange(out, desc(!!sym(name)))
  } else {
    out
  }
}

#' @export
#' @rdname count
add_count <- function(x, ..., wt = NULL, sort = FALSE, name = NULL, .drop = deprecated()) {
  if (!missing(.drop)) {
    lifecycle::deprecate_warn("1.0.0", "add_count(.drop = )")
  }

  if (!missing(...)) {
    out <- group_by(x, ..., .add = TRUE)
  } else {
    out <- x
  }
  out <- add_tally(out, wt = !!enquo(wt), sort = sort, name = name)
  if (is.data.frame(x)) {
    out <- dplyr_reconstruct(out, x)
  }
  out
}

#' @rdname count
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
      abort(c(
        "Column `n` is already present in output.",
        i = "Use `name = \"new_name\"` to pick a new name."
      ))
    }
    return("n")
  }

  if (!is.character(name) || length(name) != 1) {
    abort("`name` must be a single string.")
  }

  name
}
