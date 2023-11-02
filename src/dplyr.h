#ifndef DPLYR_DPLYR_H
#define DPLYR_DPLYR_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rversion.h>

#define UTF8_MASK (1<<3)
#define ASCII_MASK (1<<6)

#define IS_ASCII(x) (LEVELS(x) & ASCII_MASK)
#define IS_UTF8(x) (LEVELS(x) & UTF8_MASK)

#if (R_VERSION < R_Version(3, 5, 0))
# define LOGICAL_RO(x) ((const int*) LOGICAL(x))
# define INTEGER_RO(x) ((const int*) INTEGER(x))
# define REAL_RO(x) ((const double*) REAL(x))
# define COMPLEX_RO(x) ((const Rcomplex*) COMPLEX(x))
# define STRING_PTR_RO(x) ((const SEXP*) STRING_PTR(x))
# define RAW_RO(x) ((const Rbyte*) RAW(x))
# define DATAPTR_RO(x) ((const void*) STRING_PTR(x))
#endif

#define VECTOR_PTR_RO(x) ((const SEXP*) DATAPTR_RO(x))

namespace dplyr {

struct envs {
  static SEXP ns_dplyr;
  static SEXP ns_vctrs;
  static SEXP ns_rlang;
};

struct symbols {
  static SEXP groups;
  static SEXP levels;
  static SEXP ptype;
  static SEXP current_group_id;
  static SEXP current_group_size;
  static SEXP current_expression;
  static SEXP rows;
  static SEXP caller;
  static SEXP current_data;
  static SEXP dot_drop;
  static SEXP dplyr_internal_error;
  static SEXP dplyr_internal_signal;
  static SEXP chops;
  static SEXP obj_is_list;
  static SEXP new_env;
  static SEXP dot_data;
  static SEXP used;
  static SEXP across;
  static SEXP env_current_group_info;
  static SEXP env_mask_bindings;
};

struct vectors {
  static SEXP classes_vctrs_list_of;
  static SEXP empty_int_vector;

  static SEXP names_expanded;
  static SEXP names_summarise_recycle_chunks;
};

struct functions {
  static SEXP vec_chop;
  static SEXP dot_subset2;
  static SEXP list;
  static SEXP function;
};

} // namespace dplyr

namespace rlang {
SEXP eval_tidy(SEXP expr, SEXP data, SEXP env);
SEXP as_data_pronoun(SEXP x);
SEXP new_data_mask(SEXP bottom, SEXP top);
SEXP str_as_symbol(SEXP);
SEXP quo_get_expr(SEXP quo);
void env_unbind(SEXP, SEXP);
}

namespace vctrs {
bool obj_is_vector(SEXP x) ;
R_len_t short_vec_size(SEXP x) ;
SEXP short_vec_recycle(SEXP x, R_len_t n);

inline bool obj_is_list(SEXP x) {
  SEXP call = PROTECT(Rf_lang2(dplyr::symbols::obj_is_list, x));
  SEXP res = Rf_eval(call, dplyr::envs::ns_vctrs);
  UNPROTECT(1);
  return LOGICAL(res)[0];
}

}

SEXP ffi_dplyr_reconstruct(SEXP data, SEXP template_);
SEXP ffi_test_dplyr_attributes(SEXP x);
SEXP ffi_test_dplyr_set_attributes(SEXP x, SEXP attributes);

SEXP dplyr_expand_groups(SEXP old_groups, SEXP positions, SEXP s_nr);
SEXP dplyr_cumall(SEXP x);
SEXP dplyr_cumany(SEXP x);
SEXP dplyr_cummean(SEXP x);
SEXP dplyr_validate_grouped_df(SEXP df, SEXP s_check_bounds);
SEXP dplyr_validate_rowwise_df(SEXP df);
SEXP dplyr_mask_eval_all(SEXP quo, SEXP env_private);
SEXP dplyr_mask_eval_all_summarise(SEXP quo, SEXP env_private);
SEXP dplyr_mask_eval_all_mutate(SEXP quo, SEXP env_private);
SEXP dplyr_mask_eval_all_filter(SEXP quos, SEXP env_private, SEXP s_n, SEXP env_filter);
SEXP dplyr_summarise_recycle_chunks_in_place(SEXP list_of_chunks, SEXP list_of_result);
SEXP dplyr_group_indices(SEXP data, SEXP rows);
SEXP dplyr_group_keys(SEXP group_data);

SEXP dplyr_mask_binding_remove(SEXP env_private, SEXP s_name);
SEXP dplyr_mask_binding_add(SEXP env_private, SEXP s_name, SEXP ptype, SEXP chunks);

SEXP dplyr_lazy_vec_chop(SEXP data, SEXP rows, SEXP env_current_group_info, SEXP ffi_grouped, SEXP ffi_rowwise);
SEXP dplyr_make_mask_bindings(SEXP chops, SEXP data);
SEXP env_resolved(SEXP env, SEXP names);
void add_mask_binding(SEXP name, SEXP env_mask_bindings, SEXP env_chops);

SEXP dplyr_extract_chunks(SEXP df_list, SEXP df_ptype);

#define DPLYR_MASK_INIT()                                                                                            \
  SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));                                         \
  const SEXP* v_rows = VECTOR_PTR_RO(rows);                                                                          \
  R_xlen_t ngroups = XLENGTH(rows);                                                                                  \
  SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));                                     \
  SEXP env_mask_bindings = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::env_mask_bindings));               \
  SEXP pronoun = PROTECT(rlang::as_data_pronoun(env_mask_bindings));                                                 \
  SEXP env_current_group_info = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::env_current_group_info));     \
  SEXP current_group_id = PROTECT(Rf_findVarInFrame(env_current_group_info, dplyr::symbols::current_group_id));      \
  int* p_current_group_id = INTEGER(current_group_id);                                                               \
  *p_current_group_id = 0;                                                                                           \
  SEXP current_group_size = PROTECT(Rf_findVarInFrame(env_current_group_info, dplyr::symbols::current_group_size));  \
  int* p_current_group_size = INTEGER(current_group_size);                                                           \
  *p_current_group_size = 0

#define DPLYR_MASK_FINALISE()                                  \
  UNPROTECT(7);                                                \
  *p_current_group_id = 0;                                     \
  *p_current_group_size = 0

// At each iteration, we create a fresh data mask so that lexical side effects,
// such as using `<-` in a `mutate()`, don't persist between groups
#define DPLYR_MASK_ITERATION_INIT()                                         \
  SEXP mask = PROTECT(rlang::new_data_mask(env_mask_bindings, R_NilValue)); \
  Rf_defineVar(dplyr::symbols::dot_data, pronoun, mask)

#define DPLYR_MASK_ITERATION_FINALISE()                        \
  UNPROTECT(1)

#define DPLYR_MASK_SET_GROUP(INDEX)                            \
  *p_current_group_id = INDEX + 1;                             \
  *p_current_group_size = Rf_xlength(v_rows[INDEX])

#define DPLYR_MASK_EVAL(quo)                                   \
  rlang::eval_tidy(quo, mask, caller)

#define DPLYR_ERROR_INIT(n)                                    \
  SEXP error_data = PROTECT(Rf_allocVector(VECSXP, n));              \
  SEXP error_names = PROTECT(Rf_allocVector(STRSXP, n));             \
  Rf_setAttrib(error_data, R_NamesSymbol, error_names);

#define DPLYR_ERROR_SET(i, name, value)                        \
  SET_VECTOR_ELT(error_data, i, value);                        \
  SET_STRING_ELT(error_names, i, Rf_mkChar(name));

#define DPLYR_ERROR_THROW(klass)                                    \
  SEXP error_class = PROTECT(Rf_mkString(klass));              \
  SEXP error_call = PROTECT(Rf_lang3(dplyr::symbols::dplyr_internal_error, error_class, error_data)); \
  Rf_eval(error_call, dplyr::envs::ns_dplyr);                  \
  UNPROTECT(4) ; // for rchk

#endif
