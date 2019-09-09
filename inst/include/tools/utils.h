#ifndef dplyr_tools_utils_H
#define dplyr_tools_utils_H

SEXP shared_SEXP(SEXP x);

bool is_vector(SEXP x);
bool is_atomic(SEXP x);

SEXP vec_names(SEXP x);
SEXP vec_names_or_empty(SEXP x);
bool is_str_empty(SEXP str);
bool has_name_at(SEXP x, R_len_t i);

namespace dplyr {
namespace internal {

// *INDENT-OFF*
struct rlang_api_ptrs_t {
  SEXP (*quo_get_expr)(SEXP quo);
  SEXP (*quo_set_expr)(SEXP quo, SEXP expr);
  SEXP (*quo_get_env)(SEXP quo);
  SEXP (*quo_set_env)(SEXP quo, SEXP env);
  SEXP (*new_quosure)(SEXP expr, SEXP env);
  bool (*is_quosure)(SEXP x);
  SEXP (*as_data_pronoun)(SEXP data);
  SEXP (*as_data_mask)(SEXP data, SEXP parent);
  SEXP (*new_data_mask)(SEXP bottom, SEXP top);
  SEXP (*eval_tidy)(SEXP expr, SEXP data, SEXP env);

  rlang_api_ptrs_t() {
    quo_get_expr =      (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_quo_get_expr");
    quo_set_expr =      (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_quo_set_expr");
    quo_get_env =       (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_quo_get_env");
    quo_set_env =       (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_quo_set_env");
    new_quosure =       (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_new_quosure");
    is_quosure =        (bool (*)(SEXP))             R_GetCCallable("rlang", "rlang_is_quosure");
    as_data_pronoun =   (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_as_data_pronoun");
    as_data_mask =      (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_as_data_mask");
    new_data_mask =     (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_new_data_mask_3.0.0");
    eval_tidy =         (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rlang", "rlang_eval_tidy");
  }
};
// *INDENT-ON*

const rlang_api_ptrs_t& rlang_api();

} // namespace internal


} // dplyr

namespace rlang {

inline SEXP quo_get_expr(SEXP quo) {
  return dplyr::internal::rlang_api().quo_get_expr(quo);
}

inline SEXP quo_set_expr(SEXP quo, SEXP expr) {
  return dplyr::internal::rlang_api().quo_set_expr(quo, expr);
}

inline SEXP quo_get_env(SEXP quo) {
  return dplyr::internal::rlang_api().quo_get_env(quo);
}

inline bool is_quosure(SEXP x) {
  return dplyr::internal::rlang_api().is_quosure(x);
}

inline SEXP new_data_mask(SEXP bottom, SEXP top) {
  return dplyr::internal::rlang_api().new_data_mask(bottom, top);
}

inline SEXP as_data_pronoun(SEXP data) {
  return dplyr::internal::rlang_api().as_data_pronoun(data);
}

inline SEXP eval_tidy(SEXP expr, SEXP data, SEXP env) {
  return dplyr::internal::rlang_api().eval_tidy(expr, data, env);
}

inline SEXP new_quosure(SEXP expr, SEXP env) {
  return dplyr::internal::rlang_api().new_quosure(expr, env);
}

}

#endif // #ifndef dplyr_tools_utils_H
