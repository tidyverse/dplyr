#' Return rows with matching conditions
#'
#' Use `filter()` find rows/cases where conditions are true. Unlike
#' base subsetting, rows where the condition evaluates to `NA` are dropped.
#'
#' Note that dplyr is not yet smart enough to optimise filtering optimisation
#' on grouped datasets that don't need grouped calculations. For this reason,
#' filtering is often considerably faster on [ungroup()]ed data.
#'
#' @section Useful filter functions:
#'
#' * [`==`], [`>`], [`>=`] etc
#' * [`&`], [`|`], [`!`], [xor()]
#' * [is.na()]
#' * [between()], [near()]
#'
#' @section Tidy data:
#' When applied to a data frame, row names are silently dropped. To preserve,
#' convert to an explicit variable with [tibble::rownames_to_column()].
#'
#' @family single table verbs
#' @param .data A tbl. All main verbs are S3 generics and provide methods
#'   for [tbl_df()], [dtplyr::tbl_dt()] and [tbl_sql()].
#' @param ... Logical predicates defined in terms of the variables in `.data`.
#'   Multiple conditions are combined with `&`. Only rows where the
#'   conditon evalutes to `TRUE` are kept.
#' @param .dots Used to work around non-standard evaluation. See
#'   `vignette("nse")` for details.
#' @return An object of the same class as `.data`.
#' @export
#' @examples
#' filter(starwars, species == "Human")
#' filter(starwars, mass > 1000)
#'
#' # Multiple criteria
#' filter(starwars, hair_color == "none" & eye_color == "black")
#' filter(starwars, hair_color == "none" | eye_color == "black")
#'
#' # Multiple arguments are equivalent to and
#' filter(starwars, hair_color == "none", eye_color == "black")
filter <- function(.data, ...) {
  filter_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname filter
filter_ <- function(.data, ..., .dots) {
  UseMethod("filter_")
}

#' Select rows by position
#'
#' Slice does not work with relational databases because they have no
#' intrinsic notion of row order. If you want to perform the equivalent
#' operation, use [filter()] and [row_number()].
#'
#' @family single table verbs
#' @param .data A tbl. All main verbs are S3 generics and provide methods
#'   for [tbl_df()], [dtplyr::tbl_dt()] and [tbl_sql()].
#' @param ... Integer row values
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @export
#' @examples
#' slice(mtcars, 1L)
#' slice(mtcars, n())
#' slice(mtcars, 5:n())
#'
#' by_cyl <- group_by(mtcars, cyl)
#' slice(by_cyl, 1:2)
#'
#' # Equivalent code using filter that will also work with databases,
#' # but won't be as fast for in-memory data. For many databases, you'll
#' # need to supply an explicit variable to use to compute the row number.
#' filter(mtcars, row_number() == 1L)
#' filter(mtcars, row_number() == n())
#' filter(mtcars, between(row_number(), 5, n()))
slice <- function(.data, ...) {
  slice_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname slice
slice_ <- function(.data, ..., .dots) {
  UseMethod("slice_")
}

#' Summarise multiple values to a single value
#'
#' `summarise()` is typically used on grouped data created by [group_by()].
#' The output will have one row for each group.
#'
#' @section Useful functions:
#'
#' * Center: [mean()], [median()]
#' * Spread: [sd()], [IQR()], [mad()]
#' * Range: [min()], [max()], [quantile()]
#' * Position: [first()], [last()], [nth()],
#' * Count: [n()], [n_distinct()]
#' * Logical: [any()], [all()]
#'
#' @section Backend variations:
#'
#' Data frames are the only backend that supports creating a variable and
#' using it in the same summary. See examples for more details.
#'
#' @export
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... Name-value pairs of summary functions. The name will be the
#'   name of the variable in the result. The value should be an expression
#'   that returns a single value like `min(x)`, `n()`, or `sum(is.na(y))`.
#' @family single table verbs
#' @return An object of the same class as `.data`. One grouping level will
#'   be dropped.
#' @examples
#' # A summary applied to ungrouped tbl returns a single row
#' mtcars %>%
#'   summarise(mean = mean(disp), n = n())
#'
#' # Usually, you'll want to group first
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(mean = mean(disp), n = n())
#'
#' # Each summary call removes one grouping level (since that group
#' # is now just a single row)
#' mtcars %>%
#'   group_by(cyl, vs) %>%
#'   summarise(cyl_n = n()) %>%
#'   group_vars()
#'
#' # Note that with data frames, newly created summaries immediately
#' # overwrite existing variables
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(disp = mean(disp), sd = sd(disp))
summarise <- function(.data, ...) {
  UseMethod("summarise")
}
#' @export
#' @rdname summarise
summarise_ <- function(.data, ..., .dots) {
  UseMethod("summarise_")
}

#' @rdname summarise
#' @export
summarize <- summarise
#' @rdname summarise
#' @export
summarize_ <- summarise_


#' Add new variables
#'
#' `mutate()` adds new variables and preserves existing;
#' `transmute()` drops existing variables.
#'
#' @section Useful functions:
#'
#' * [`+`], [`-`] etc
#'
#' * [log()]
#'
#' * [lead()], [lag()]
#'
#' * [dense_rank()], [min_rank()], [percent_rank()], [row_number()],
#'   [cume_dist()], [ntile()]
#'
#' * [cumsum()], [cummean()], [cummin()], [cummax()], [cumany()], [cumall()]
#'
#' * [na_if()], [coalesce()]
#'
#' * [if_else()], [recode()], [case_when()]
#'
#' @export
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... Name-value pairs of expressions. Use `NULL` to drop
#'   a variable.
#' @family single table verbs
#' @return An object of the same class as `.data`.
#' @examples
#' # Newly created variables are available immediately
#' mtcars %>% mutate(
#'   cyl2 = cyl * 2,
#'   cyl4 = cyl2 * 2
#' )
#'
#' # window functions are useful for grouped mutates
#' mtcars %>%
#'  group_by(cyl) %>%
#'  mutate(rank = min_rank(desc(mpg)))
#' # see `vignette("window-functions")` for more details
#'
#' # You can drop variables by setting them to NULL
#' mtcars %>% mutate(cyl = NULL)
#'
#' # mutate() vs transmute --------------------------
#' # mutate() keeps all existing variables
#' mtcars %>%
#'   mutate(displ_l = disp / 61.0237)
#'
#' # transmute keeps only the variables you create
#' mtcars %>%
#'   transmute(displ_l = disp / 61.0237)
#'
mutate <- function(.data, ...) {
  UseMethod("mutate")
}
#' @export
#' @rdname mutate
mutate_ <- function(.data, ..., .dots) {
  UseMethod("mutate_")
}

#' @rdname mutate
#' @export
transmute <- function(.data, ...) {
  transmute_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @rdname mutate
#' @export
transmute_ <- function(.data, ..., .dots) {
  UseMethod("transmute_")
}

#' @export
transmute_.default <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  out <- mutate_(.data, .dots = dots)

  keep <- names(dots)
  select(out, one_of(keep))
}

#' Arrange rows by variables
#'
#' Use [desc()] to sort a variable in descending order.
#'
#' @section Locales:
#' The sort order for character vectors will depend on the collating sequence
#' of the locale in use: see [locales()].
#'
#' @export
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... Comma separated list of unquoted variable names. Use
#'   [desc()] to sort a variable in descending order.
#' @family single table verbs
#' @return An object of the same class as `.data`.
#' @examples
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, desc(disp))
arrange <- function(.data, ...) {
  UseMethod("arrange")
}
#' @export
#' @rdname arrange
arrange_ <- function(.data, ..., .dots) {
  UseMethod("arrange_")
}

#' Select/rename variables by name
#'
#' `select()` keeps only the variables you mention; `rename()`
#' keeps all variables.
#'
#' @section Useful functions:
#' As well as using existing functions like `:` and `c()`, there are
#' a number of special functions that only work inside `select`
#'
#' * [starts_with()], [ends_with()], [contains()]
#' * [matches()]
#' * [num_range()]
#'
#' To drop variables, use `-`.
#'
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... Comma separated list of unquoted expressions. You can treat
#'   variable names like they are positions.
#'
#'   Positive values select variables; negative values to drop variables.
#'
#'   Use named arguments to rename selected variables.
#' @param .dots Use `select_()` to do standard evaluation. See
#'   `vignette("nse")` for details
#' @return An object of the same class as `.data`.
#' @family single table verbs
#' @export
#' @examples
#' iris <- as_tibble(iris) # so it prints a little nicer
#' select(iris, starts_with("Petal"))
#' select(iris, ends_with("Width"))
#'
#' # Move Species variable to the front
#' select(iris, Species, everything())
#'
#' df <- as.data.frame(matrix(runif(100), nrow = 10))
#' df <- tbl_df(df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)])
#' select(df, V4:V6)
#' select(df, num_range("V", 4:6))
#'
#' # Drop variables with -
#' select(iris, -starts_with("Petal"))
#'
#' # Renaming -----------------------------------------
#' # * select() keeps only the variables you specify
#' select(iris, petal_length = Petal.Length)
#'
#' # * rename() keeps all variables
#' rename(iris, petal_length = Petal.Length)
select <- function(.data, ...) {
  UseMethod("select")
}
#' @export
#' @rdname select
select_ <- function(.data, ..., .dots) {
  UseMethod("select_")
}

#' Select columns using a predicate
#'
#' This verb is analogous to [summarise_if()] and
#' [mutate_if()] in that it lets you use a predicate on
#' the columns of a data frame. Only those columns for which the
#' predicate returns `TRUE` will be selected.
#'
#' Predicates can only be used with local sources like a data frame.
#'
#' @inheritParams summarise_all
#' @param .data A local tbl source.
#' @param ... Additional arguments passed to `.predicate`.
#' @export
#' @examples
#' iris %>% select_if(is.factor)
#' iris %>% select_if(is.numeric)
#' iris %>% select_if(function(col) is.numeric(col) && mean(col) > 3.5)
select_if <- function(.data, .predicate, ...) {
  if (inherits(.data, "tbl_lazy")) {
    abort("Selection with predicate currently require local sources")
  }
  vars <- probe_colwise_names(.data, .predicate, ...)
  vars <- ensure_grouped_vars(vars, .data, notify = FALSE)
  select(.data, !!! symbols(vars))
}

#' @rdname select
#' @export
rename <- function(.data, ...) {
  rename_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @rdname select
#' @export
rename_ <- function(.data, ..., .dots) {
  UseMethod("rename_")
}

#' The number of observations in the current group.
#'
#' This function is implemented specifically for each data source and can only
#' be used from within [summarise()], [mutate()] and
#' [filter()].
#'
#' @export
#' @examples
#' if (require("nycflights13")) {
#' carriers <- group_by(flights, carrier)
#' summarise(carriers, n())
#' mutate(carriers, n = n())
#' filter(carriers, n() < 100)
#' }
n <- function() {
  abort("This function should not be called directly")
}
