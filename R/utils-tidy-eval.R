#' Other tidy eval tools
#'
#' @description
#' These tidy eval functions are no longer for normal usage, but are still
#' exported from dplyr for backward compatibility.
#' See [`?dplyr_tidy_eval`][dplyr_tidy_eval] and `vignette("programming")` for
#' latest recommendations.
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
#' @aliases expr enquo enquos sym syms as_label
#' @export expr enquo enquos sym syms as_label .data
#' @aliases quo quos quo_name ensym ensyms enexpr enexprs
#' @export quo quos quo_name ensym ensyms enexpr enexprs
NULL
