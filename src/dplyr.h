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
  static SEXP dot_current_group;
  static SEXP current_expression;
  static SEXP rows;
  static SEXP caller;
  static SEXP all_vars;
  static SEXP dot_drop;
  static SEXP abort_glue;
  static SEXP dot_indices;
  static SEXP chops;
  static SEXP mask;
  static SEXP rm;
  static SEXP envir;
  static SEXP vec_is_list;
  static SEXP new_env;
  static SEXP dot_data;
=======
  static SEXP used;
  static SEXP filter_combine;
  static SEXP across;
>>>>>>> 765632200 (+if_any() / if_all())
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
}

namespace vctrs {
bool vec_is_vector(SEXP x) ;
R_len_t short_vec_size(SEXP x) ;
SEXP short_vec_recycle(SEXP x, R_len_t n);

inline bool vec_is_list(SEXP x) {
  SEXP call = PROTECT(Rf_lang2(dplyr::symbols::vec_is_list, x));
  SEXP res = Rf_eval(call, dplyr::envs::ns_vctrs);
  UNPROTECT(1);
  return LOGICAL(res)[0];
}

}

SEXP dplyr_expand_groups(SEXP old_groups, SEXP positions, SEXP s_nr);
SEXP dplyr_filter_update_rows(SEXP s_n_rows, SEXP group_indices, SEXP keep, SEXP new_rows_sizes);
SEXP dplyr_between(SEXP x, SEXP s_left, SEXP s_right);
SEXP dplyr_cumall(SEXP x);
SEXP dplyr_cumany(SEXP x);
SEXP dplyr_cummean(SEXP x);
SEXP dplyr_validate_grouped_df(SEXP df, SEXP s_check_bounds);
SEXP dplyr_mask_eval_all(SEXP quo, SEXP env_private);
SEXP dplyr_mask_eval_all_summarise(SEXP quo, SEXP env_private);
SEXP dplyr_mask_eval_all_mutate(SEXP quo, SEXP env_private);
SEXP dplyr_mask_eval_all_filter(SEXP quos, SEXP env_private, SEXP s_n, SEXP env_filter);
SEXP dplyr_summarise_recycle_chunks(SEXP chunks, SEXP rows, SEXP ptypes);
SEXP dplyr_group_indices(SEXP data, SEXP rows);
SEXP dplyr_group_keys(SEXP group_data);

SEXP dplyr_mask_remove(SEXP env_private, SEXP s_name);
SEXP dplyr_mask_add(SEXP env_private, SEXP s_name, SEXP chunks);

SEXP dplyr_lazy_vec_chop(SEXP data, SEXP rows);
SEXP dplyr_data_masks_setup(SEXP chops, SEXP data, SEXP rows);
SEXP env_resolved(SEXP env, SEXP names);
void add_mask_binding(SEXP name, SEXP env_bindings, SEXP env_chops);

#define DPLYR_MASK_INIT()                                                                    \
SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));                   \
R_xlen_t ngroups = XLENGTH(rows);                                                            \
SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));               \
SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));                   \
SEXP chops_env = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::chops));               \
SEXP current_group = PROTECT(Rf_findVarInFrame(ENCLOS(chops_env), dplyr::symbols::dot_current_group)) ;\
int* p_current_group = INTEGER(current_group)

#define DPLYR_MASK_FINALISE() UNPROTECT(5)

#define DPLYR_MASK_SET_GROUP(INDEX) *p_current_group = INDEX + 1

#define DPLYR_MASK_EVAL(quo) rlang::eval_tidy(quo, mask, caller)

#define DPLYR_ERROR_INIT(n)                                    \
  SEXP error_data = PROTECT(Rf_allocVector(VECSXP, n));              \
  SEXP error_names = PROTECT(Rf_allocVector(STRSXP, n));             \
  Rf_setAttrib(error_data, R_NamesSymbol, error_names);

#define DPLYR_ERROR_MESG_INIT(n)                               \
  SEXP error_message = PROTECT(Rf_allocVector(STRSXP, n));     \

#define DPLYR_ERROR_SET(i, name, value)                        \
  SET_VECTOR_ELT(error_data, i, value);                        \
  SET_STRING_ELT(error_names, i, Rf_mkChar(name));

#define DPLYR_ERROR_MSG_SET(i, msg)                        \
  SET_STRING_ELT(error_message, i, Rf_mkChar(msg));                          \

#define DPLYR_ERROR_THROW(klass)                                    \
  SEXP error_class = PROTECT(Rf_mkString(klass));              \
  SEXP error_call = PROTECT(Rf_lang4(dplyr::symbols::abort_glue, error_message, error_data, error_class)); \
  Rf_eval(error_call, dplyr::envs::ns_dplyr);                  \
  UNPROTECT(5) ; // for rchk

#endif
