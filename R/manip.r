#' Return rows with matching conditions
#'
#' Use `filter()` to choose rows/cases where conditions are true. Unlike
#' base subsetting with `[`, rows where the condition evaluates to `NA` are
#' dropped.
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
#' @section Grouped tibbles:
#'
#' Because filtering expressions are computed within groups, they may
#' yield different results on grouped tibbles. This will be the case
#' as soon as an aggregating, lagging, or ranking function is
#' involved. Compare this ungrouped filtering:
#'
#' ```
#' starwars %>% filter(mass > mean(mass, na.rm = TRUE))
#' ```
#'
#' With the grouped equivalent:
#'
#' ```
#' starwars %>% group_by(gender) %>% filter(mass > mean(mass, na.rm = TRUE))
#' ```
#'
#' The former keeps rows with `mass` greater than the global average
#' whereas the latter keeps rows with `mass` greater than the gender
#' average.
#'
#' It is valid to use grouping variables in filter expressions.
#'
#' When applied on a grouped tibble, `filter()` automatically [rearranges][arrange]
#' the tibble by groups for performance reasons.
#'
#' @section Tidy data:
#' When applied to a data frame, row names are silently dropped. To preserve,
#' convert to an explicit variable with [tibble::rownames_to_column()].
#'
#' @section Scoped filtering:
#' The three [scoped] variants ([filter_all()], [filter_if()] and
#' [filter_at()]) make it easy to apply a filtering condition to a
#' selection of variables.
#'
#' @family single table verbs
#' @param .data A tbl. All main verbs are S3 generics and provide methods
#'   for [tbl_df()], [dtplyr::tbl_dt()] and [dbplyr::tbl_dbi()].
#' @param ... Logical predicates defined in terms of the variables in `.data`.
#'   Multiple conditions are combined with `&`. Only rows where the
#'   condition evaluates to `TRUE` are kept.
#'
#'   The arguments in `...` are automatically [quoted][rlang::quo] and
#'   [evaluated][rlang::eval_tidy] in the context of the data
#'   frame. They support [unquoting][rlang::quasiquotation] and
#'   splicing. See `vignette("programming")` for an introduction to
#'   these concepts.
#' @param .preserve when `FALSE` (the default), the grouping structure
#'   is recalculated based on the resulting data, otherwise it is kept as is.
#' @return An object of the same class as `.data`.
#' @seealso [filter_all()], [filter_if()] and [filter_at()].
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
#'
#'
#' # The filtering operation may yield different results on grouped
#' # tibbles because the expressions are computed within groups.
#' #
#' # The following filters rows where `mass` is greater than the
#' # global average:
#' starwars %>% filter(mass > mean(mass, na.rm = TRUE))
#'
#' # Whereas this keeps rows with `mass` greater than the gender
#' # average:
#' starwars %>% group_by(gender) %>% filter(mass > mean(mass, na.rm = TRUE))
#'
#'
#' # Refer to column names stored as strings with the `.data` pronoun:
#' vars <- c("mass", "height")
#' cond <- c(80, 150)
#' starwars %>%
#'   filter(
#'     .data[[vars[[1]]]] > cond[[1]],
#'     .data[[vars[[2]]]] > cond[[2]]
#'   )
#'
#' # For more complex cases, knowledge of tidy evaluation and the
#' # unquote operator `!!` is required. See https://tidyeval.tidyverse.org/
#' #
#' # One useful and simple tidy eval technique is to use `!!` to bypass
#' # the data frame and its columns. Here is how to filter the columns
#' # `mass` and `height` relative to objects of the same names:
#' mass <- 80
#' height <- 150
#' filter(starwars, mass > !!mass, height > !!height)
filter <- function(.data, ..., .preserve = FALSE) {
  UseMethod("filter")
}
#' @export
filter.default <- function(.data, ..., .preserve = FALSE) {
  filter_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
filter_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "filter_() is deprecated. ",
    "Please use filter() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with filter() : https://tidyeval.tidyverse.org"
  ))
  UseMethod("filter_")
}

#' Choose rows by position
#'
#' Choose rows by their ordinal position in the tbl.  Grouped tbls use
#' the ordinal position within the group.
#'
#' Slice does not work with relational databases because they have no
#' intrinsic notion of row order. If you want to perform the equivalent
#' operation, use [filter()] and [row_number()].
#'
#' @family single table verbs
#' @param .data A tbl.
#' @param ... Integer row values.  Provide either positive values to keep,
#'   or negative values to drop. The values provided must be either all
#'   positive or all negative.  Indices beyond the number of rows in the
#'   input are silently ignored.
#'
#'   The arguments in `...` are automatically [quoted][rlang::quo] and
#'   [evaluated][rlang::eval_tidy] in the context of the data
#'   frame. They support [unquoting][rlang::quasiquotation] and
#'   splicing. See `vignette("programming")` for an introduction to
#'   these concepts.
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @export
#' @examples
#' slice(mtcars, 1L)
#' # Similar to tail(mtcars, 1):
#' slice(mtcars, n())
#' slice(mtcars, 5:n())
#' # Rows can be dropped with negative indices:
#' slice(mtcars, -5:-n())
#' # In this case, the result will be equivalent to:
#' slice(mtcars, 1:4)
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
slice <- function(.data, ..., .preserve = FALSE) {
  UseMethod("slice")
}
#' @export
slice.default <- function(.data, ..., .preserve = FALSE) {
  slice_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
slice_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "slice_() is deprecated. ",
    "Please use slice() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with slice() : https://tidyeval.tidyverse.org"
  ))
  UseMethod("slice_")
}

#' Reduce multiple values down to a single value
#'
#' Create one or more scalar variables summarizing the variables of an
#' existing tbl. Tbls with groups created by [group_by()] will result in one
#' row in the output for each group.  Tbls with no groups will result in one row.
#'
#' `summarise()` and `summarize()` are synonyms.
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
#' The data frame backend supports creating a variable and using it in the
#' same summary. This means that previously created summary variables can be
#' further transformed or combined within the summary, as in [mutate()].
#' However, it also means that summary variables with the same names as previous
#' variables overwrite them, making those variables unavailable to later summary
#' variables.
#'
#' This behaviour may not be supported in other backends. To avoid unexpected
#' results, consider using new names for your summary variables, especially when
#' creating multiple summaries.
#'
#' @export
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... Name-value pairs of summary functions. The name will be the
#'   name of the variable in the result. The value should be an expression
#'   that returns a single value like `min(x)`, `n()`, or `sum(is.na(y))`.
#'
#'   The arguments in `...` are automatically [quoted][rlang::quo] and
#'   [evaluated][rlang::eval_tidy] in the context of the data
#'   frame. They support [unquoting][rlang::quasiquotation] and
#'   splicing. See `vignette("programming")` for an introduction to
#'   these concepts.
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
#'
#' # Reusing variable names when summarising may lead to unexpected results
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarise(disp = mean(disp), sd = sd(disp), double_disp = disp * 2)
#'
#'
#' # Refer to column names stored as strings with the `.data` pronoun:
#' var <- "mass"
#' summarise(starwars, avg = mean(.data[[var]], na.rm = TRUE))
#'
#' # For more complex cases, knowledge of tidy evaluation and the
#' # unquote operator `!!` is required. See https://tidyeval.tidyverse.org/
#' #
#' # One useful and simple tidy eval technique is to use `!!` to
#' # bypass the data frame and its columns. Here is how to divide the
#' # column `mass` by an object of the same name:
#' mass <- 100
#' summarise(starwars, avg = mean(mass / !!mass, na.rm = TRUE))
summarise <- function(.data, ...) {
  UseMethod("summarise")
}
#' @export
summarise.default <- function(.data, ...) {
  summarise_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
summarise_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "summarise_() is deprecated. ",
    "Please use summarise() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with summarise() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("summarise_")
}

#' @rdname summarise
#' @export
summarize <- summarise
#' @rdname se-deprecated
#' @export
summarize_ <- summarise_


#' Create or transform variables
#'
#' `mutate()` adds new variables and preserves existing ones;
#' `transmute()` adds new variables and drops existing ones.  Both
#' functions preserve the number of rows of the input.
#' New variables overwrite existing variables of the same name.
#'
#' @section Useful functions available in calculations of variables:
#'
#' * [`+`], [`-`], [log()], etc., for their usual mathematical meanings
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
#' @section Grouped tibbles:
#'
#' Because mutating expressions are computed within groups, they may
#' yield different results on grouped tibbles. This will be the case
#' as soon as an aggregating, lagging, or ranking function is
#' involved. Compare this ungrouped mutate:
#'
#' ```
#' starwars %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#' ```
#'
#' With the grouped equivalent:
#'
#' ```
#' starwars %>%
#'   group_by(gender) %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#' ```
#'
#' The former normalises `mass` by the global average whereas the
#' latter normalises by the averages within gender levels.
#'
#' Note that you can't overwrite a grouping variable within
#' `mutate()`.
#'
#' `mutate()` does not evaluate the expressions when the group is empty.
#'
#' @section Scoped mutation and transmutation:
#'
#' The three [scoped] variants of `mutate()` ([mutate_all()],
#' [mutate_if()] and [mutate_at()]) and the three variants of
#' `transmute()` ([transmute_all()], [transmute_if()],
#' [transmute_at()]) make it easy to apply a transformation to a
#' selection of variables.
#'
#' @export
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... Name-value pairs of expressions, each with length 1 or the same
#'   length as the number of rows in the group (if using [group_by()]) or in the entire
#'   input (if not using groups). The name of each argument will be the name of
#'   a new variable, and the value will be its corresponding value.  Use a `NULL`
#'   value in `mutate` to drop a variable.  New variables overwrite existing variables
#'   of the same name.
#'
#'   The arguments in `...` are automatically [quoted][rlang::quo] and
#'   [evaluated][rlang::eval_tidy] in the context of the data
#'   frame. They support [unquoting][rlang::quasiquotation] and
#'   splicing. See `vignette("programming")` for an introduction to
#'   these concepts.
#' @family single table verbs
#' @return An object of the same class as `.data`.
#' @examples
#' # Newly created variables are available immediately
#' mtcars %>% as_tibble() %>% mutate(
#'   cyl2 = cyl * 2,
#'   cyl4 = cyl2 * 2
#' )
#'
#' # You can also use mutate() to remove variables and
#' # modify existing variables
#' mtcars %>% as_tibble() %>% mutate(
#'   mpg = NULL,
#'   disp = disp * 0.0163871 # convert to litres
#' )
#'
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
#'
#' # The mutate operation may yield different results on grouped
#' # tibbles because the expressions are computed within groups.
#' # The following normalises `mass` by the global average:
#' starwars %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#'
#' # Whereas this normalises `mass` by the averages within gender
#' # levels:
#' starwars %>%
#'   group_by(gender) %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#'
#' # Note that you can't overwrite grouping variables:
#' gdf <- mtcars %>% group_by(cyl)
#' try(mutate(gdf, cyl = cyl * 100))
#'
#'
#' # Refer to column names stored as strings with the `.data` pronoun:
#' vars <- c("mass", "height")
#' mutate(starwars, prod = .data[[vars[[1]]]] * .data[[vars[[2]]]])
#'
#' # For more complex cases, knowledge of tidy evaluation and the
#' # unquote operator `!!` is required. See https://tidyeval.tidyverse.org/
#' #
#' # One useful and simple tidy eval technique is to use `!!` to
#' # bypass the data frame and its columns. Here is how to divide the
#' # column `mass` by an object of the same name:
#' mass <- 100
#' mutate(starwars, mass = mass / !!mass)
mutate <- function(.data, ...) {
  UseMethod("mutate")
}
#' @export
mutate.default <- function(.data, ...) {
  mutate_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
mutate_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "mutate_() is deprecated. ",
    "Please use mutate() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with mutate() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("mutate_")
}

#' @rdname mutate
#' @export
transmute <- function(.data, ...) {
  UseMethod("transmute")
}
#' @rdname se-deprecated
#' @export
transmute_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "transmute_() is deprecated. ",
    "Please use transmute() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with transmute() : https://tidyeval.tidyverse.org"
  ))
  UseMethod("transmute_")
}

#' @export
transmute.default <- function(.data, ...) {
  dots <- enquos(..., .named = TRUE)
  out <- mutate(.data, !!!dots)

  keep <- names(dots)
  select(out, one_of(keep))
}
#' @export
transmute_.default <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  transmute(.data, !!!dots)
}

#' @export
transmute.grouped_df <- function(.data, ...) {
  dots <- enquos(..., .named = TRUE)
  out <- mutate(.data, !!!dots)
  keep <- names(dots)

  .select_grouped_df(out, one_of(keep), notify = FALSE)
}
#' @export
transmute_.grouped_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  transmute(.data, !!!dots)
}


#' Arrange rows by variables
#'
#' Order tbl rows by an expression involving its variables.
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
#' @param ... Comma separated list of unquoted variable names, or expressions
#'   involving variable names. Use [desc()] to sort a variable in descending order.
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
#' @export
arrange.default <- function(.data, ...) {
  arrange_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
arrange_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "arrange_() is deprecated. ",
    "Please use arrange() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with arrange() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("arrange_")
}

#' Select/rename variables by name
#'
#' Choose or rename variables from a tbl.
#' `select()` keeps only the variables you mention; `rename()`
#' keeps all variables.
#'
#' These functions work by column index, not value; thus, an expression
#' like `select(data.frame(x = 1:5, y = 10), z = x+1)` does not create a variable
#' with values `2:6`. (In the current implementation, the expression `z = x+1`
#' wouldn't do anything useful.)  To calculate using column values, see
#' [mutate()]/[transmute()].
#'
#' @section Useful functions:
#' As well as using existing functions like `:` and `c()`, there are
#' a number of special functions that only work inside `select()`:
#'
#' * [starts_with()], [ends_with()], [contains()]
#' * [matches()]
#' * [num_range()]
#' * [one_of()]
#' * [everything()]
#' * [group_cols()]
#'
#' To drop variables, use `-`.
#'
#' Note that except for `:`, `-` and `c()`, all complex expressions
#' are evaluated outside the data frame context. This is to prevent
#' accidental matching of data frame variables when you refer to
#' variables from the calling context.
#'
#' @section Scoped selection and renaming:
#'
#' The three [scoped] variants of `select()` ([select_all()],
#' [select_if()] and [select_at()]) and the three variants of
#' `rename()` ([rename_all()], [rename_if()], [rename_at()]) make it
#' easy to apply a renaming function to a selection of variables.
#'
#' @inheritParams filter
#' @inheritSection filter Tidy data
#' @param ... One or more unquoted expressions separated by commas.
#'   You can treat variable names like they are positions, so you can
#'   use expressions like `x:y` to select ranges of variables.
#'
#'   Positive values select variables; negative values drop variables.
#'   If the first expression is negative, `select()` will automatically
#'   start with all variables.
#'
#'   Use named arguments, e.g. `new_name = old_name`, to rename selected variables.
#'
#'   The arguments in `...` are automatically [quoted][rlang::quo] and
#'   [evaluated][rlang::eval_tidy] in a context where column names
#'   represent column positions. They also support
#'   [unquoting][rlang::quasiquotation] and splicing. See
#'   `vignette("programming")` for an introduction to these concepts.
#'
#'   See [select helpers][tidyselect::select_helpers] for more details and
#'   examples about tidyselect helpers such as `starts_with()`, `everything()`, ...
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
#' # Move Sepal.Length variable to back
#' # first select all variables except Sepal.Length, then re select Sepal.Length
#' select(iris, -Sepal.Length, Sepal.Length)
#'
#' df <- as.data.frame(matrix(runif(100), nrow = 10))
#' df <- tbl_df(df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)])
#' select(df, V4:V6)
#' select(df, num_range("V", 4:6))
#'
#' # Drop variables with -
#' select(iris, -starts_with("Petal"))
#'
#' # Select the grouping variables:
#' starwars %>% group_by(gender) %>% select(group_cols())
#'
#'
#' # The .data pronoun is available:
#' select(mtcars, .data$cyl)
#' select(mtcars, .data$mpg : .data$disp)
#'
#' # However it isn't available within calls since those are evaluated
#' # outside of the data context. This would fail if run:
#' # select(mtcars, identical(.data$cyl))
#'
#'
#' # Renaming -----------------------------------------
#' # * select() keeps only the variables you specify
#' select(iris, petal_length = Petal.Length)
#'
#' # * rename() keeps all variables
#' rename(iris, petal_length = Petal.Length)
#'
#' # * select() can rename variables in a group
#' select(iris, obs = starts_with('S'))
#'
#' # Unquoting ----------------------------------------
#'
#' # Like all dplyr verbs, select() supports unquoting of symbols:
#' vars <- list(
#'   var1 = sym("cyl"),
#'   var2 = sym("am")
#' )
#' select(mtcars, !!!vars)
#'
#' # For convenience it also supports strings and character
#' # vectors. This is unlike other verbs where strings would be
#' # ambiguous.
#' vars <- c(var1 = "cyl", var2 ="am")
#' select(mtcars, !!vars)
#' rename(mtcars, !!vars)
select <- function(.data, ...) {
  UseMethod("select")
}
#' @export
select.default <- function(.data, ...) {
  select_(.data, .dots = compat_as_lazy_dots(...))
}
#' @export
#' @rdname se-deprecated
select_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "select_() is deprecated. ",
    "Please use select() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with select() : https://tidyeval.tidyverse.org"
  ))

  UseMethod("select_")
}

#' @export
select.list <- function(.data, ...) {
  abort("`select()` doesn't handle lists.")
}

#' @rdname select
#' @export
rename <- function(.data, ...) {
  UseMethod("rename")
}
#' @export
rename.default <- function(.data, ...) {
  rename_(.data, .dots = compat_as_lazy_dots(...))
}
#' @rdname se-deprecated
#' @export
rename_ <- function(.data, ..., .dots = list()) {
  signal_soft_deprecated(paste_line(
    "rename_() is deprecated. ",
    "Please use rename() instead",
    "",
    "The 'programming' vignette or the tidyeval book can help you",
    "to program with rename() : https://tidyeval.tidyverse.org"
  ))

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
  from_context("..group_size")
}

#' Deprecated SE versions of main verbs.
#'
#' dplyr used to offer twin versions of each verb suffixed with an
#' underscore. These versions had standard evaluation (SE) semantics:
#' rather than taking arguments by code, like NSE verbs, they took
#' arguments by value. Their purpose was to make it possible to
#' program with dplyr. However, dplyr now uses tidy evaluation
#' semantics. NSE verbs still capture their arguments, but you can now
#' unquote parts of these arguments. This offers full programmability
#' with NSE verbs. Thus, the underscored versions are now superfluous.
#'
#' Unquoting triggers immediate evaluation of its operand and inlines
#' the result within the captured expression. This result can be a
#' value or an expression to be evaluated later with the rest of the
#' argument. See `vignette("programming")` for more information.
#'
#' @name se-deprecated
#' @param .data A data frame.
#' @param dots,.dots,... Pair/values of expressions coercible to lazy objects.
#' @param vars Various meanings depending on the verb.
#' @param args Various meanings depending on the verb.
#' @keywords internal
NULL
