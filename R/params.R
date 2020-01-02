#' Argument type: tidy-eval
#'
#' @description
#' This page the argument modifier `<tidy-eval>`. This indicates that the
#' argument uses **tidy evaluation** with **data masking**; a special type of
#' non-standard evaluation used throughout the tidyverse.
#'
#' # Key terms
#'
#' The primary motivation for tidy evaluation is that it provides data-masking,
#' which blurs the distinction between two types of variables:
#'
#' * __env-variables__ are "programming" variables and live in an environment.
#'   They are usually created with `<-`. Env-variables can be any type of R
#'   object.
#'
#' * __data-variables__ are "statistical" variables and live in a data frame.
#'   They usually come from data files (e.g. `csv`), or are created by
#'   manipulating existing variables. Data-variables live inside data frames,
#'   so must be vectors.
#'
#' # Referring to variables indirectly
#'
#' There are two main cases:
#'
#' * If you have the column name as a character vector, use the `.data`
#'   pronoun: `.data[[var]]`.
#'
#' * If you want the user to supply the colum name in a function call,
#'   __embrace__ the argument name: `{{ var }}`.
#'
#' # Dot-dot-dot (...)
#'
#' When this modifier is applied to `...`, there is one additional technique
#' which solves the problem of creating a new variable with a name supplied
#' by the user. Use `!!var := expression`. (Note the use of `:=` instead of
#' `=`).
#'
#' @keywords internal
#' @name dplyr_tidy_eval
NULL


#' Argument type: tidy-select
#'
#' @description
#' This page the argument modifier `<tidy-select>`. This indicates that the
#' argument uses the tidyselect syntax which provides a concise DSL for
#' selecting variables based on their names.
#'
#' @keywords internal
#' @name dplyr_tidy_select
NULL
