#' Count observations by group
#'
#' @description
#' `count()` lets you quickly count the unique values of one or more variables:
#' `df %>% count(a, b)` is roughly equivalent to
#' `df %>% group_by(a, b) %>% summarise(n = n())`.
#' `count()` is paired with `tally()`, a lower-level helper that is equivalent
#' to `df %>% summarise(n = n())`. Supply `wt` to perform weighted counts,
#' switching the summary from `n = n()` to `n = sum(wt)`.
#'
#' `add_count()` and `add_tally()` are equivalents to `count()` and `tally()`
#' but use `mutate()` instead of `summarise()` so that they add a new column
#' with group-wise counts.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a
#'   lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... <[`data-masking`][dplyr_data_masking]> Variables to group by.
#' @param wt <[`data-masking`][dplyr_data_masking]> Frequency weights.
#'   Can be `NULL` or a variable:
#'
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param name The name of the new column in the output.
#'
#'   If omitted, it will default to `n`. If there's already a column called `n`,
#'   it will error, and require you to specify the name.
#' @param .drop For `count()`: if `FALSE` will include counts for empty groups
#'   (i.e. for levels of factors that don't exist in the data). Deprecated in
#'   `add_count()` since it didn't actually affect the output.
#' @param x `r lifecycle::badge("deprecated")` This argument has been renamed 
#'    to `.data` to fit dplyr's terminology and is deprecated.
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
count <- function(.data, ..., wt = NULL, sort = FALSE, name = NULL, x = deprecated()) {
  if (lifecycle::is_present(x)) {
    lifecycle::deprecate_warn("1.0.8", "count(x)", "count(.data)")
    return(count_dispatch(
      .data = x, !!enexpr(.data), ..., wt = !!enquo(wt),
      sort = sort, name = name
    ))
  }
  UseMethod("count")
}

#' @export
count.data.frame <- function(.data, ..., wt = NULL, sort = FALSE, name = NULL, .drop = group_by_drop_default(.data),
                             x = deprecated()) {
  if (!missing(...)) {
    out <- group_by(.data, ..., .add = TRUE, .drop = .drop)
  } else {
    out <- .data 
  }

  out <- tally(out, wt = !!enquo(wt), sort = sort, name = name)

  # Ensure grouping is transient
  if (is.data.frame(.data)) {
    out <- dplyr_reconstruct(out, .data)
  }
  out
}

count.tbl_sql <- count.data.frame

#' @export
#' @rdname count
tally <- function(.data, wt = NULL, sort = FALSE, name = NULL, x = deprecated()) {
  UseMethod("tally")
}

#' @export
tally.data.frame <- function(.data, wt = NULL, sort = FALSE, name = NULL, x = deprecated()) {
  if (lifecycle::is_present(x)) {
    lifecycle::deprecate_warn("1.0.8", "tally(x)", "tally(.data)")
    .data <- x
  }

  n <- tally_n(.data, {{ wt }})
  name <- check_name(name, group_vars(.data))

  out <- local({
    old.options <- options(dplyr.summarise.inform = FALSE)
    on.exit(options(old.options))
    summarise(.data, !!name := !!n)
  })

  if (sort) {
    arrange(out, desc(!!sym(name)))
  } else {
    out
  }
}

tally.tbl_sql <- tally.data.frame

#' @export
#' @rdname count
add_count <- function(.data, ..., wt = NULL, sort = FALSE, name = NULL, .drop = deprecated(),
                      x = deprecated()) {
  if (lifecycle::is_present(x)) {
    lifecycle::deprecate_warn("1.0.8", "count(x)", "add_count(.data)")
    return(add_count_dispatch(
      .data = x, !!enexpr(.data), ..., wt = !!enquo(wt),
      sort = sort, name = name
    ))
  }

  UseMethod("add_count")
}

#' @export
add_count.default <- function(.data, ..., wt = NULL, sort = FALSE, name = NULL, .drop = deprecated(),
                              x = deprecated()) {
  if (!missing(.drop)) {
    lifecycle::deprecate_warn("1.0.0", "add_count(.drop = )")
  }

  if (!missing(...)) {
    out <- group_by(.data, ..., .add = TRUE)
  } else {
    out <- .data
  }
  add_tally(out, wt = !!enquo(wt), sort = sort, name = name)
}


#' @export
add_count.data.frame <- function(.data, ..., wt = NULL, sort = FALSE, name = NULL, .drop = deprecated(),
                                 x = deprecated()) {
  if (!missing(.drop)) {
    lifecycle::deprecate_warn("1.0.0", "add_count(.drop = )")
  }

  if (!missing(...)) {
    out <- group_by(.data, ..., .add = TRUE)
  } else {
    out <- .data
  }
  out <- add_tally(out, wt = !!enquo(wt), sort = sort, name = name)
  dplyr_reconstruct(out, .data)
}

#' @rdname count
#' @export
add_tally <- function(.data, wt = NULL, sort = FALSE, name = NULL, x = deprecated()) {
  if (lifecycle::is_present(x)) {
    lifecycle::deprecate_warn("1.0.8", "add_tally(x)", "add_tally(.data)")
    .data <- x
  }

  n <- tally_n(.data, {{ wt }})
  name <- check_name(name, tbl_vars(.data))
  out <- mutate(.data, !!name := !!n)

  if (sort) {
    arrange(out, desc(!!sym(name)))
  } else {
    out
  }
}

# Helpers -----------------------------------------------------------------

count_dispatch <- function(.data, ..., wt = NULL, sort = FALSE, name = NULL) {
  UseMethod("count")
}

add_count_dispatch <- function(.data, ..., wt = NULL, sort = FALSE, name = NULL) {
  UseMethod("add_count")
}

tally_n <- function(.data, wt) {
  wt <- enquo(wt)

  if (is_call(quo_get_expr(wt), "n", n = 0)) {
    # Provided only by dplyr 1.0.0. See #5349 for discussion.
    warn(c(
      "`wt = n()` is deprecated",
      i = "You can now omit the `wt` argument"
    ))
    wt <- quo(NULL)
  }

  if (quo_is_null(wt)) {
    expr(n())
  } else {
    expr(sum(!!wt, na.rm = TRUE))
  }
}

check_name <- function(name, vars) {
  if (is.null(name)) {
    name <- n_name(vars)

    if (name != "n") {
      inform(c(
        glue("Storing counts in `{name}`, as `n` already present in input"),
        i = "Use `name = \"new_name\"` to pick a new name."
      ))
    }
  } else if (!is.character(name) || length(name) != 1) {
    abort("`name` must be a single string.")
  }

  name
}

n_name <- function(x) {
  name <- "n"
  while (name %in% x) {
    name <- paste0("n", name)
  }

  name
}
