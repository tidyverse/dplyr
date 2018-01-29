#include <Rcpp.h>


namespace dplyr {
namespace internal {

// rlang callables
SEXP (*rlang_quo_get_expr)(SEXP quo) = NULL;
SEXP (*rlang_quo_set_expr)(SEXP quo, SEXP expr) = NULL;
SEXP (*rlang_quo_get_env)(SEXP quo) = NULL;
SEXP (*rlang_quo_set_env)(SEXP quo, SEXP env) = NULL;
SEXP (*rlang_new_quosure)(SEXP expr, SEXP env) = NULL;
SEXP (*rlang_is_quosure)(SEXP x) = NULL;
SEXP (*rlang_as_data_pronoun)(SEXP data) = NULL;
SEXP (*rlang_as_data_mask)(SEXP data, SEXP parent) = NULL;
SEXP (*rlang_new_data_mask)(SEXP bottom, SEXP top, SEXP parent) = NULL;

} // namespace internal
} // namespace dplyr


// [[Rcpp::export]]
SEXP dplyr_onload() {
  dplyr::internal::rlang_quo_get_expr =     (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_quo_get_expr");
  dplyr::internal::rlang_quo_set_expr =     (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_quo_set_expr");
  dplyr::internal::rlang_quo_get_env =      (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_quo_get_env");
  dplyr::internal::rlang_quo_set_env =      (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_quo_set_env");
  dplyr::internal::rlang_new_quosure =      (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_new_quosure");
  dplyr::internal::rlang_is_quosure =       (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_is_quosure");
  dplyr::internal::rlang_as_data_pronoun =  (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_as_data_pronoun");
  dplyr::internal::rlang_as_data_mask =     (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_as_data_mask");
  dplyr::internal::rlang_new_data_mask =    (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rlang", "rlang_new_data_mask");
  return R_NilValue;
}
