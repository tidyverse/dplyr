#' Return rows with matching conditions.
#'
#' @family single table verbs
#' @param .data A tbl. All main verbs are S3 generics and provide methods
#'   for \code{\link{tbl_df}}, \code{\link[dtplyr]{tbl_dt}} and \code{\link{tbl_sql}}.
#' @param ... Logical predicates. Multiple conditions are combined with \code{&}.
#' @param .dots Used to work around non-standard evaluation. See
#'   \code{vignette("nse")} for details.
#' @return An object of the same class as \code{.data}.
#'
#'   Data frame row names are silently dropped. To preserve, convert to an
#'   explicit variable.
#' @export
#' @examples
#' filter(mtcars, cyl == 8)
#' filter(mtcars, cyl < 6)
#'
#' # Multiple criteria
#' filter(mtcars, cyl < 6 & vs == 1)
#' filter(mtcars, cyl < 6 | vs == 1)
#'
#' # Multiple arguments are equivalent to and
#' filter(mtcars, cyl < 6, vs == 1)
filter <- function(.data, ...) {
  filter_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname filter
filter_ <- function(.data, ..., .dots) {
  UseMethod("filter_")
}

#' Select rows by position.
#'
#' Slice does not work with relational databases because they have no
#' intrinsic notion of row order. If you want to perform the equivalent
#' operation, use \code{\link{filter}()} and \code{\link{row_number}()}.
#'
#' @family single table verbs
#' @param .data A tbl. All main verbs are S3 generics and provide methods
#'   for \code{\link{tbl_df}}, \code{\link[dtplyr]{tbl_dt}} and \code{\link{tbl_sql}}.
#' @param ... Integer row values
#' @inheritParams filter
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

#' Summarise multiple values to a single value.
#'
#' @section Backend variations:
#'
#' Data frames are the only backend that supports creating a variable and
#' using it in the same summary. See examples for more details.
#'
#' @export
#' @inheritParams filter
#' @param ... Name-value pairs of summary functions like \code{\link{min}()},
#'   \code{\link{mean}()}, \code{\link{max}()} etc.
#' @family single table verbs
#' @return An object of the same class as \code{.data}. One grouping level will
#'   be dropped.
#'
#'   Data frame row names are silently dropped. To preserve, convert to an
#'   explicit variable.
#' @examples
#' summarise(mtcars, mean(disp))
#' summarise(group_by(mtcars, cyl), mean(disp))
#' summarise(group_by(mtcars, cyl), m = mean(disp), sd = sd(disp))
#'
#' # With data frames, you can create and immediately use summaries
#' by_cyl <- mtcars %>% group_by(cyl)
#' by_cyl %>% summarise(a = n(), b = a + 1)
#'
#' \dontrun{
#' # You can't with data tables or databases
#' by_cyl_dt <- mtcars %>% dtplyr::tbl_dt() %>% group_by(cyl)
#' by_cyl_dt %>% summarise(a = n(), b = a + 1)
#'
#' by_cyl_db <- src_sqlite(":memory:", create = TRUE) %>%
#'   copy_to(mtcars) %>% group_by(cyl)
#' by_cyl_db %>% summarise(a = n(), b = a + 1)
#' }
summarise <- function(.data, ...) {
  summarise_(.data, .dots = lazyeval::lazy_dots(...))
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


#' Add new variables.
#'
#' Mutate adds new variables and preserves existing; transmute drops existing
#' variables.
#'
#' @export
#' @inheritParams filter
#' @param ... Name-value pairs of expressions. Use \code{NULL} to drop
#'   a variable.
#' @family single table verbs
#' @return An object of the same class as \code{.data}.
#'
#'   Data frame row names are silently dropped. To preserve, convert to an
#'   explicit variable.
#' @examples
#' mutate(mtcars, displ_l = disp / 61.0237)
#' transmute(mtcars, displ_l = disp / 61.0237)
#'
#' mutate(mtcars, cyl = NULL)
mutate <- function(.data, ...) {
  mutate_(.data, .dots = lazyeval::lazy_dots(...))
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

#' Arrange rows by variables.
#'
#' Use \code{\link{desc}} to sort a variable in descending order. Generally,
#' this will not also automatically order by grouping variables.
#'
#' @section Locales:
#'
#' Note that for local data frames, the ordering is done in C++ code which
#' does not have access to the local specific ordering usually done in R.
#' This means that strings are ordered as if in the C locale.
#'
#' @export
#' @inheritParams filter
#' @param ... Comma separated list of unquoted variable names. Use
#'   \code{\link{desc}} to sort a variable in descending order.
#' @family single table verbs
#' @return An object of the same class as \code{.data}.
#'
#'   Data frame row names are silently dropped. To preserve, convert to an
#'   explicit variable.
#' @examples
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, desc(disp))
arrange <- function(.data, ...) {
  arrange_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname arrange
arrange_ <- function(.data, ..., .dots) {
  UseMethod("arrange_")
}

#' Select/rename variables by name.
#'
#' \code{select()} keeps only the variables you mention; \code{rename()}
#' keeps all variables.
#'
#' @section Special functions:
#' As well as using existing functions like \code{:} and \code{c}, there are
#' a number of special functions that only work inside \code{select}
#'

#'
#' To drop variables, use \code{-}. You can rename variables with
#' named arguments.
#'
#' @inheritParams filter
#' @param ... Comma separated list of unquoted expressions. You can treat
#'   variable names like they are positions. Use positive values to select
#'   variables; use negative values to drop variables.
#' @param .dots Use \code{select_()} to do standard evaluation. See
#'   \code{vignette("nse")} for details
#' @return An object of the same class as \code{.data}.
#'
#'   Data frame row names are silently dropped. To preserve, convert to an
#'   explicit variable.
#' @family single table verbs
#' @export
#' @examples
#' iris <- tbl_df(iris) # so it prints a little nicer
#' select(iris, starts_with("Petal"))
#' select(iris, ends_with("Width"))
#' select(iris, contains("etal"))
#' select(iris, matches(".t."))
#' select(iris, Petal.Length, Petal.Width)
#' vars <- c("Petal.Length", "Petal.Width")
#' select(iris, one_of(vars))
#'
#' df <- as.data.frame(matrix(runif(100), nrow = 10))
#' df <- tbl_df(df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)])
#' select(df, V4:V6)
#' select(df, num_range("V", 4:6))
#'
#' # Drop variables
#' select(iris, -starts_with("Petal"))
#' select(iris, -ends_with("Width"))
#' select(iris, -contains("etal"))
#' select(iris, -matches(".t."))
#' select(iris, -Petal.Length, -Petal.Width)
#'
#' # Rename variables:
#' # * select() keeps only the variables you specify
#' select(iris, petal_length = Petal.Length)
#' # Renaming multiple variables uses a prefix:
#' select(iris, petal = starts_with("Petal"))
#'
#' # Reorder variables: keep the variable "Species" in the front
#' select(iris, Species, everything())
#'
#' # * rename() keeps all variables
#' rename(iris, petal_length = Petal.Length)
#'
#' # Programming with select ---------------------------------------------------
#' select_(iris, ~Petal.Length)
#' select_(iris, "Petal.Length")
#' select_(iris, lazyeval::interp(~matches(x), x = ".t."))
#' select_(iris, quote(-Petal.Length), quote(-Petal.Width))
#' select_(iris, .dots = list(quote(-Petal.Length), quote(-Petal.Width)))
select <- function(.data, ...) {
  select_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
#' @rdname select
select_ <- function(.data, ..., .dots) {
  UseMethod("select_")
}

#' Select columns using a predicate
#'
#' This verb is analogous to \code{\link{summarise_if}()} and
#' \code{\link{mutate_if}()} in that it lets you use a predicate on
#' the columns of a data frame. Only those columns for which the
#' predicate returns \code{TRUE} will be selected.
#'
#' Predicates can only be used with local sources like a data frame.
#'
#' @inheritParams summarise_all
#' @param .data A local tbl source.
#' @param ... Additional arguments passed to \code{.predicate}.
#' @export
#' @examples
#' iris %>% select_if(is.factor)
#' iris %>% select_if(is.numeric)
#' iris %>% select_if(function(col) is.numeric(col) && mean(col) > 3.5)
select_if <- function(.data, .predicate, ...) {
  if (inherits(.data, "tbl_lazy")) {
    stop("Selection with predicate currently require local sources",
      call. = FALSE)
  }
  vars <- probe_colwise_names(.data, .predicate, ...)
  vars <- ensure_grouped_vars(vars, .data, notify = FALSE)
  select_(.data, .dots = vars)
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
#' This function is implemented special for each data source and can only
#' be used from within \code{\link{summarise}}, \code{\link{mutate}} and
#' \code{\link{filter}}
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
  stop("This function should not be called directly")
}
