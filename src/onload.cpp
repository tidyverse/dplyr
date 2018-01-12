#include <Rcpp.h>


// rlang callables
SEXP (*rlang_quo_get_expr)(SEXP quo);
SEXP (*rlang_quo_set_expr)(SEXP quo, SEXP expr);
SEXP (*rlang_quo_get_env)(SEXP quo);
SEXP (*rlang_quo_set_env)(SEXP quo, SEXP env);
SEXP (*rlang_new_quosure)(SEXP expr, SEXP env);
SEXP (*rlang_is_quosure)(SEXP x);
SEXP (*rlang_as_data_pronoun)(SEXP data);

// [[Rcpp::export]]
SEXP dplyr_onload() {
  rlang_quo_get_expr =     (SEXP (*)(SEXP))       R_GetCCallable("rlang", "rlang_quo_get_expr");
  rlang_quo_set_expr =     (SEXP (*)(SEXP, SEXP)) R_GetCCallable("rlang", "rlang_quo_set_expr");
  rlang_quo_get_env =      (SEXP (*)(SEXP))       R_GetCCallable("rlang", "rlang_quo_get_env");
  rlang_quo_set_env =      (SEXP (*)(SEXP, SEXP)) R_GetCCallable("rlang", "rlang_quo_set_env");
  rlang_new_quosure =      (SEXP (*)(SEXP, SEXP)) R_GetCCallable("rlang", "rlang_new_quosure");
  rlang_is_quosure =       (SEXP (*)(SEXP))       R_GetCCallable("rlang", "rlang_is_quosure");
  rlang_as_data_pronoun =  (SEXP (*)(SEXP))       R_GetCCallable("rlang", "rlang_as_data_pronoun");
  return R_NilValue;
}
