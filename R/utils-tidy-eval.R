#' Other tidy eval tools
#'
#' @description
#' These tidy eval functions are no longer for normal usage, but are still
#' exported from dplyr for backward compatibility.
#' See [`?rlang::args_data_masking`][rlang::args_data_masking] and
#' `vignette("programming")` for the latest recommendations.
#'
#' * [expr()][rlang::expr]
#' * [enquo()][rlang::enquo]
#' * [enquos()][rlang::enquos]
#' * [sym()][rlang::sym]
#' * [syms()][rlang::syms]
#' * [as_label()][rlang::as_label]
#' * [quo()][rlang::quo]
#' * [quos()][rlang::quos]
#' * [quo_name()][rlang::quo_name]
#' * [ensym()][rlang::ensym]
#' * [ensyms()][rlang::ensyms]
#' * [enexpr()][rlang::enexpr]
#' * [enexprs()][rlang::enexprs]
#'
#' @keywords internal
#' @name tidyeval-compat
#' @aliases .data expr enquo enquos sym syms as_label
#' @export .data expr enquo enquos sym syms as_label
#' @aliases quo quos quo_name ensym ensyms enexpr enexprs
#' @export quo quos quo_name ensym ensyms enexpr enexprs
NULL

# Retaining a redirect for the old `dplyr_data_masking` help page, because many
# package authors end up linking to this through inherited documentation, and
# removing the topic from here results in a check warning in their package. It
# should be possible to remove this once enough packages have re-documented with
# dplyr 1.1.1 installed and sent a new release to CRAN.
#' Data-masking
#'
#' This page is now located at
#' [`?rlang::args_data_masking`][rlang::args_data_masking].
#'
#' @keywords internal
#' @name dplyr_data_masking
NULL
