#' Count the observations in each group
#'
#' @description
#' `count()` lets you quickly count the unique values of one or more variables:
#' `df |> count(a, b)` is roughly equivalent to
#' `df |> group_by(a, b) |> summarise(n = n())`.
#' `count()` is paired with `tally()`, a lower-level helper that is equivalent
#' to `df |> summarise(n = n())`. Supply `wt` to perform weighted counts,
#' switching the summary from `n = n()` to `n = sum(wt)`.
#'
#' `add_count()` and `add_tally()` are equivalents to `count()` and `tally()`
#' but use `mutate()` instead of `summarise()` so that they add a new column
#' with group-wise counts.
#'
#' @param x A data frame, data frame extension (e.g. a tibble), or a
#'   lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variables to group
#'   by.
#' @param wt <[`data-masking`][rlang::args_data_masking]> Frequency weights.
#'   Can be `NULL` or a variable:
#'
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param name The name of the new column in the output.
#'
#'   If omitted, it will default to `n`. If there's already a column called `n`,
#'   it will use `nn`. If there's a column called `n` and `nn`, it'll use
#'   `nnn`, and so on, adding `n`s until it gets a new name.
#' @param .drop Handling of factor levels that don't appear in the data, passed
#'   on to [group_by()].
#'
#'   For `count()`: if `FALSE` will include counts for empty groups (i.e. for
#'   levels of factors that don't exist in the data).
#'
#'  `r lifecycle::badge("defunct")` For `add_count()`: defunct since it
#'  can't actually affect the output.
#' @return
#' An object of the same type as `.data`. `count()` and `add_count()`
#' group transiently, so the output has the same groups as the input.
#' @export
#' @examples
#' # count() is a convenient way to get a sense of the distribution of
#' # values in a dataset
#' starwars |> count(species)
#' starwars |> count(species, sort = TRUE)
#' starwars |> count(sex, gender, sort = TRUE)
#' starwars |> count(birth_decade = round(birth_year, -1))
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
#' df |> count(gender)
#' # counts runs:
#' df |> count(gender, wt = runs)
#'
#' # When factors are involved, `.drop = FALSE` can be used to retain factor
#' # levels that don't appear in the data
#' df2 <- tibble(
#'   id = 1:5,
#'   type = factor(c("a", "c", "a", NA, "a"), levels = c("a", "b", "c"))
#' )
#' df2 |> count(type)
#' df2 |> count(type, .drop = FALSE)
#'
#' # Or, using `group_by()`:
#' df2 |> group_by(type, .drop = FALSE) |> count()
#'
#' # tally() is a lower-level function that assumes you've done the grouping
#' starwars |> tally()
#' starwars |> group_by(species) |> tally()
#'
#' # both count() and tally() have add_ variants that work like
#' # mutate() instead of summarise
#' df |> add_count(gender, wt = runs)
#' df |> add_tally(wt = runs)
count <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  UseMethod("count")
}

#' @export
#' @rdname count
count.data.frame <- function(
  x,
  ...,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  .drop = group_by_drop_default(x)
) {
  dplyr_local_error_call()

  if (!missing(...)) {
    out <- group_by(x, ..., .add = TRUE, .drop = .drop)
  } else {
    out <- x
  }

  wt <- compat_wt(enquo(wt))
  out <- tally(out, wt = !!wt, sort = sort, name = name)

  # Ensure grouping is transient
  out <- dplyr_reconstruct(out, x)

  out
}

#' @export
#' @rdname count
tally <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  UseMethod("tally")
}

#' @export
tally.data.frame <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  name <- check_n_name(name, group_vars(x))

  dplyr_local_error_call()

  wt <- compat_wt(enquo(wt))
  n <- tally_n(x, wt)

  local_options(dplyr.summarise.inform = FALSE)
  out <- summarise(x, !!name := !!n)

  if (sort) {
    arrange(out, desc(!!sym(name)))
  } else {
    out
  }
}

#' @export
#' @rdname count
add_count <- function(
  x,
  ...,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  .drop = deprecated()
) {
  UseMethod("add_count")
}

#' @export
add_count.default <- function(
  x,
  ...,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  .drop = deprecated()
) {
  add_count_impl(
    x,
    ...,
    wt = {{ wt }},
    sort = sort,
    name = name,
    .drop = .drop
  )
}

#' @export
add_count.data.frame <- function(
  x,
  ...,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  .drop = deprecated()
) {
  out <- add_count_impl(
    x,
    ...,
    wt = {{ wt }},
    sort = sort,
    name = name,
    .drop = .drop
  )
  dplyr_reconstruct(out, x)
}

add_count_impl <- function(
  x,
  ...,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  .drop = deprecated(),
  error_call = caller_env(),
  user_env = caller_env(2)
) {
  if (!is_missing(.drop)) {
    lifecycle::deprecate_stop("1.0.0", "add_count(.drop = )", env = error_call)
  }

  dplyr_local_error_call(error_call)

  if (!missing(...)) {
    out <- group_by(x, ..., .add = TRUE)
  } else {
    out <- x
  }

  wt <- compat_wt(enquo(wt), env = error_call, user_env = user_env)
  add_tally(out, wt = !!wt, sort = sort, name = name)
}

#' @rdname count
#' @export
add_tally <- function(x, wt = NULL, sort = FALSE, name = NULL) {
  name <- check_n_name(name, tbl_vars(x))

  dplyr_local_error_call()

  wt <- compat_wt(enquo(wt))
  n <- tally_n(x, wt)
  out <- mutate(x, !!name := !!n)

  if (sort) {
    arrange(out, desc(!!sym(name)))
  } else {
    out
  }
}

# Helpers -----------------------------------------------------------------

tally_n <- function(x, wt) {
  if (quo_is_null(wt)) {
    expr(dplyr::n())
  } else {
    expr(base::sum(!!wt, na.rm = TRUE))
  }
}

compat_wt <- function(wt, env = caller_env(), user_env = caller_env(2)) {
  if (!is_call(quo_get_expr(wt), "n", n = 0)) {
    return(wt)
  }

  # Provided only by dplyr 1.0.0. See #5349 for discussion.
  lifecycle::deprecate_warn(
    when = "1.0.1",
    what = I("`wt = n()`"),
    details = "You can now omit the `wt` argument.",
    env = env,
    user_env = user_env,
    always = TRUE,
    id = "dplyr-count-wt"
  )

  quo(NULL)
}

check_n_name <- function(
  name,
  vars,
  arg = caller_arg(name),
  call = caller_env()
) {
  if (is.null(name)) {
    name <- n_name(vars)

    if (name != "n") {
      inform(c(
        glue("Storing counts in `{name}`, as `n` already present in input"),
        i = "Use `name = \"new_name\"` to pick a new name."
      ))
    }
  } else {
    check_string(name, arg = arg, call = call)
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
