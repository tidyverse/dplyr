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
#' * __env-variables__ are "programming" variables; they live in an environment,
#'   and are usually created with `<-`. Env-variables can be any type of R
#'   object.
#'
#' * __data-variables__ are "statistical" variables; they live in a data frame.
#'   Data-variables must be something that can live in a data frame, which means
#'   that they are vectors.
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
#' @keywords internal
#' @name dplyr_tidy_eval
NULL
