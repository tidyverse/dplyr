#include "dplyr.h"

namespace rlang {

// *INDENT-OFF*
struct rlang_api_ptrs_t {
  SEXP (*eval_tidy)(SEXP expr, SEXP data, SEXP env);

  rlang_api_ptrs_t() {
    eval_tidy =         (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rlang", "rlang_eval_tidy");
  }
};
// *INDENT-ON*

const rlang_api_ptrs_t& rlang_api() {
  static rlang_api_ptrs_t ptrs;
  return ptrs;
}

SEXP eval_tidy(SEXP expr, SEXP data, SEXP env) {
  return rlang_api().eval_tidy(expr, data, env);
}

}

namespace vctrs {

// *INDENT-OFF*
struct vctrs_api_ptrs_t {
  bool (*vec_is_vector)(SEXP x);
  R_len_t (*short_vec_size)(SEXP x);
  SEXP (*short_vec_recycle)(SEXP, R_len_t, struct vctrs_arg*);

  vctrs_api_ptrs_t() {
    vec_is_vector =         (bool (*)(SEXP)) R_GetCCallable("vctrs", "vec_is_vector");
    short_vec_size  =         (R_len_t (*)(SEXP)) R_GetCCallable("vctrs", "short_vec_size");
    short_vec_recycle = (SEXP (*)(SEXP, R_len_t, struct vctrs_arg*)) R_GetCCallable("vctrs", "short_vec_recycle");
  }
};
// *INDENT-ON*

const vctrs_api_ptrs_t& vctrs_api() {
  static vctrs_api_ptrs_t ptrs;
  return ptrs;
}

bool vec_is_vector(SEXP x) {
  return vctrs_api().vec_is_vector(x);
}

R_len_t short_vec_size(SEXP x) {
  return vctrs_api().short_vec_size(x);
}

SEXP short_vec_recycle(SEXP x, R_len_t n) {
  return vctrs_api().short_vec_recycle(x, n, NULL);
}

}
