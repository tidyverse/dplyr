#' Argument type: tidy-select
#'
#' @description
#' This page describes the `<tidy-select>` argument modifier which indicates
#' the argument supports **tidy selections**. Tidy selection provides a concise
#' dialect of R for selecting variables based on their names or properties.
#'
#' Tidy selection is a variant of tidy evaluation. This means that inside
#' functions, tidy-select arguments require special attention, as described in
#' the *Indirection* section below. If you've never heard of tidy evaluation
#' before, start with `vignette("programming")`.
#'
#'
#' # Overview of selection features
#'
#' ```{r, child = "man/rmd/overview.Rmd"}
#' ```
#'
#'
#' # Indirection
#'
#' There are two main cases:
#'
#' *   If you have a character vector of column names, use `all_of()`
#'     or `any_of()`, depending on whether or not you want unknown variable
#'     names to cause an error, e.g. `select(df, all_of(vars))`,
#'     `select(df, !any_of(vars))`.
#'
#' *   If you want the user to be able to supply a tidyselect specification in
#'     a function argument, embrace the function argument, e.g.
#'     `select(df, {{ vars }})`.
#'
#' @keywords internal
#' @name dplyr_tidy_select
NULL
