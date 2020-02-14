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
  SEXP (*vec_assign_impl)(SEXP, SEXP, SEXP, bool);
  SEXP (*vec_cast)(SEXP, SEXP, SEXP, SEXP);
  SEXP (*short_vec_init)(SEXP, R_len_t);

  vctrs_api_ptrs_t() {
    vec_is_vector =         (bool (*)(SEXP)) R_GetCCallable("vctrs", "vec_is_vector");
    short_vec_size  =         (R_len_t (*)(SEXP)) R_GetCCallable("vctrs", "short_vec_size");
    vec_assign_impl = (SEXP (*)(SEXP, SEXP, SEXP, bool)) R_GetCCallable("vctrs", "vec_assign_impl");
    vec_cast = (SEXP (*)(SEXP, SEXP, SEXP, SEXP)) R_GetCCallable("vctrs", "vctrs_cast");
    short_vec_init = (SEXP (*)(SEXP, R_len_t)) R_GetCCallable("vctrs", "short_vec_init");
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

SEXP vec_assign_impl(SEXP x, SEXP index, SEXP value, bool clone) {
  return vctrs_api().vec_assign_impl(x, index, value, clone);
}

SEXP vec_cast(SEXP x, SEXP to, SEXP x_arg_, SEXP to_arg_) {
  return vctrs_api().vec_cast(x, to, x_arg_, to_arg_);
}

SEXP short_vec_init(SEXP x, R_len_t n) {
  return vctrs_api().short_vec_init(x, n);
}
}
